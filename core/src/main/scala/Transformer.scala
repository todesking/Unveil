package com.todesking.unveil

import scala.language.existentials

import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

trait Transformer { self =>
  def name: String
  def params: Map[String, String] = Map.empty

  def apply[A <: AnyRef](orig: Instance.Concrete[A], el: EventLogger): Try[Instance.Duplicate[A]] =
    try {
      el.enterTransformer(this, orig) { el => Success(apply0(orig, el)) }
    } catch {
      case e: UnveilException =>
        el.fail(e)
        Failure(e)
    }
  protected[this] def apply0[A <: AnyRef](orig: Instance.Concrete[A], el: EventLogger): Instance.Duplicate[A]

  def andThen(next: Transformer): Transformer =
    new Transformer {
      override def name = s"${self.name} >>> ${next.name}"
      override def apply[A <: AnyRef](orig: Instance.Concrete[A], el: EventLogger): Try[Instance.Duplicate[A]] = {
        el.enterTransformer(this, orig) { el =>
          self.apply(orig, el).flatMap { i2 => next.apply(i2, el) }
        }
      }
      override def apply0[A <: AnyRef](orig: Instance.Concrete[A], el: EventLogger) = throw new AssertionError()
    }

  def >>>(next: Transformer): Transformer =
    this andThen next
}
object Transformer {
  // TODO: remove this
  def newEventLogger(): EventLogger =
    new EventLogger

  // TODO: support instance-stateful fields(need leakage detection)
  // TODO: support mutable fields(if fref eq original then optimized else original)
  object fieldFusion extends Transformer {
    override def name = s"fieldFusion"
    override def apply0[A <: AnyRef](instance: Instance.Concrete[A], el: EventLogger): Instance.Duplicate[A] = {
      val dupInstance = instance.duplicate1(el)
      fuse(
        "",
        dupInstance,
        dupInstance
          .rewritableVirtualMethods
          .keySet
          .filterNot { mr => dupInstance.resolveVirtualMethod(mr) == ClassRef.Object }
          .map { mr => dupInstance.resolveVirtualMethod(mr) -> mr } ++ (
            dupInstance.klass
            .declaredMethods
            .keySet
            .map { mr => dupInstance.thisRef -> mr }
          ),
        el
      )
    }
    // TODO: prevent inf loop
    private[this] def fuse[A <: AnyRef](
      memberPrefix: String,
      self: Instance.Duplicate[A],
      methods: Set[(ClassRef, MethodRef)],
      el: EventLogger
    ): Instance.Duplicate[A] = {
      val dfs =
        methods
          .toSeq
          .map { case k @ (cr, mr) => k -> self.dataflow(cr, mr) }
          .toMap
      val usedFields: Set[(ClassRef, FieldRef)] =
        dfs.values
          .map(_.usedFieldsOf(self))
          .reduceLeftOption(_ ++ _).getOrElse(Set.empty)
          .filter { case (cr, fr) => self.fields(cr -> fr).attribute.isFinal }
      el.logCFields("used fields from target methods", usedFields)

      usedFields.foldLeft(self) {
        case (self, (fcr, fr)) =>
          el.enterField(fcr, fr) { el =>
            self.fields(fcr -> fr).data match {
              // TODO[refactor]: instance.isInstanceStateful
              case Data.ConcreteReference(fieldInstance) if !fieldInstance.fields.forall(_._2.attribute.isFinal) =>
                el.log("Pass: This field is instance-stateful")
                self
              case Data.ConcreteReference(fieldInstance) =>
                // TODO: log
                val usedMethods = fieldInstance.klass.extendMethods(
                  methods.flatMap {
                    case (cr, mr) =>
                      self.dataflow(cr, mr).usedMethodsOf(fieldInstance)
                  }
                )
                el.logCMethods("used methods in the field", usedMethods)

                val methodRenaming =
                  usedMethods.map {
                    case k @ (cr, mr) =>
                      k -> mr.anotherUniqueName(memberPrefix + fr.name, mr.name)
                  }.toMap
                val fieldRenaming =
                  fieldInstance.fields.keys.map {
                    case k @ (cr, fr1) =>
                      k -> fr1.anotherUniqueName(memberPrefix + fr.name, fr1.name)
                  }.toMap
                val newFields = fieldRenaming.map { case ((cr, fr), nfr) => nfr -> fieldInstance.fields(cr, fr) }
                val newMethods =
                  usedMethods
                    .toIterable
                    .map { case k @ (cr, mr) => k -> fieldInstance.dataflow(cr, mr) }
                    .toMap
                    .map {
                      case ((cr, mr), df) =>
                        methodRenaming(cr -> mr) -> df.body.rewrite {
                          case (label, bc: Bytecode.InvokeInstanceMethod) if df.mustThis(label, bc.objectref) =>
                            Bytecode.invokespecial(self.thisRef, methodRenaming(bc.classRef, bc.methodRef))
                          case (label, bc: Bytecode.InstanceFieldAccess) if df.mustThis(label, bc.objectref) =>
                            bc.rewriteFieldRef(self.thisRef, fieldRenaming(bc.classRef, bc.fieldRef))
                        }.makePrivate
                    }
                val rewrittenMethods =
                  methods
                    .map {
                      case (cr, mr) =>
                        val df = self.dataflow(cr, mr)
                        import Bytecode._
                        val leaked =
                          df.body.bytecode
                            .exists {
                              case (label, bc: Bytecode.Shuffle) => false
                              case (label, bc: Bytecode.Control) => false
                              case (label, bc @ getfield(_, _)) => false
                              case (label, bc @ putfield(_, _)) =>
                                df.dataSource(label, bc.value).mayFieldAccess(fcr, fr)
                              case (label, bc: InvokeMethod) =>
                                bc.args.exists { arg => df.dataSource(label, arg).mayFieldAccess(fcr, fr) }
                              case _ => false
                            }
                        if (leaked) {
                          el.log(s"[SKIP] the field is leaked in method $mr")
                          mr -> df.body
                        } else {
                          // TODO: use df.mustFieldRef instead of df.mustInstance when rewriting
                          mr -> df.body.rewrite {
                            case (label, bc @ getfield(cr1, fr1)) if df.mustThis(label, bc.objectref) && self.resolveField(cr1, fr1) == cr && fr1 == fr =>
                              nop()
                            case (label, bc: InvokeInstanceMethod) if df.mustInstance(label, bc.objectref, fieldInstance) =>
                              methodRenaming.get(fieldInstance.resolveVirtualMethod(bc.methodRef) -> bc.methodRef).fold {
                                throw new AssertionError(s"Can't find renamed method for ${bc.classRef}.${bc.methodRef}")
                              } { mr =>
                                invokespecial(self.thisRef, mr)
                              }
                            case (label, bc: InstanceFieldAccess) if df.mustInstance(label, bc.objectref, fieldInstance) =>
                              fieldRenaming.get(fieldInstance.resolveField(bc.classRef, bc.fieldRef) -> bc.fieldRef).fold(bc) {
                                case fr =>
                                  bc.rewriteFieldRef(self.thisRef, fr)
                              }
                          }
                        }
                    }.toMap
                val newSelf = self.addFields(newFields).addMethods(newMethods).addMethods(rewrittenMethods)
                el.section(s"Fuse new methods from ${fr.name}") { el =>
                  fuse(memberPrefix + fr.name + "__", newSelf, newMethods.keys.map { case mr => (self.thisRef -> mr) }.toSet, el)
                }
              case _ =>
                el.log("Pass")
                self
            }
          }
      }
    }
  }

  object methodInlining extends Transformer {
    override def name = "methodInlining"
    override def apply0[A <: AnyRef](orig: Instance.Concrete[A], el: EventLogger): Instance.Duplicate[A] = {
      orig
        .rewritableVirtualMethods
        .keys
        .filterNot { mr => orig.resolveVirtualMethod(mr) == ClassRef.Object }
        .foldLeft(orig.duplicate1(el)) {
          case (self, mr) =>
            val cr = self.resolveVirtualMethod(mr)
            el.log(s"Inlining $mr")
            val inlined =
              el.enterMethod(cr, mr) { el => inline(self.dataflow(cr, mr), Set(cr -> mr), el) }
            self.addMethod(mr, inlined)
        }
    }

    private[this] def inline(df: DataFlow, ignore: Set[(ClassRef, MethodRef)], el: EventLogger): MethodBody = {
      // TODO: if(df.localModified(0)) df.body
      var localOffset = df.maxLocals
      import Bytecode._
      df.body.rewrite_* {
        case (label, bc: InvokeInstanceMethod) if df.mustThis(label, bc.objectref) =>
          el.section(s"Inline invocation of ${bc.classRef}.${bc.methodRef}") { el =>
            val mr = bc.methodRef
            val cr =
              bc match {
                case invokespecial(cr, mr) =>
                  // TODO[BUG]: resolve special
                  cr
                case invokevirtual(cr, mr) =>
                  df.instance.resolveVirtualMethod(mr)
                case invokeinterface(cr, mr, _) =>
                  df.instance.resolveVirtualMethod(mr)
              }
            val calleeDf =
              if (ignore.contains(cr -> mr)) df.instance.dataflow(cr, mr)
              else inline(df.instance.dataflow(cr, mr), ignore + (cr -> mr), el).dataflow(df.instance)

            // TODO[BUG]: if(calleeDf.localModified(0)) ...
            val argOffset = if (calleeDf.body.isStatic) localOffset else localOffset + 1
            // TODO: support exception
            val cf =
              calleeDf.body.rewrite_* {
                case (_, bc: LocalAccess) =>
                  CodeFragment.bytecode(bc.rewriteLocalIndex(bc.localIndex + localOffset))
                case (label, bc: XReturn) =>
                  // TODO: [BUG] goto tail
                  val resultLocal = localOffset + calleeDf.maxLocals
                  CodeFragment.bytecode(
                    Seq(autoStore(bc.returnType, resultLocal)) ++ (
                      calleeDf.beforeFrames(label).stack.drop(bc.returnType.wordSize).map {
                        case FrameItem(src, d) =>
                          autoPop(d.typeRef)
                      }
                    ) ++ Seq(
                      autoLoad(bc.returnType, resultLocal)
                    )
                    : _*
                  )
                case (label, bc: VoidReturn) =>
                  CodeFragment.bytecode(
                    calleeDf.beforeFrames(label).stack.map {
                      case FrameItem(src, d) =>
                        autoPop(d.typeRef)
                    }: _*
                  )
              }.codeFragment
                .prepend(
                  CodeFragment.bytecode(
                    mr.descriptor.args.reverse.zipWithIndex.map { case (t, i) => autoStore(t, i + argOffset) } ++
                      (if (calleeDf.body.isStatic) Seq.empty else Seq(astore(localOffset)))
                      : _*
                  )
                )
            localOffset += calleeDf.maxLocals + 1 // TODO: inefficient if static method
            cf
          }
      }
    }
  }

  object localInstanceInlining extends Transformer {
    override def name = "localInstanceInlining"
    override def apply0[A <: AnyRef](orig: Instance.Concrete[A], el: EventLogger): Instance.Duplicate[A] = {
      orig
        .rewritableVirtualMethods
        .keys
        .filterNot { mr => orig.resolveVirtualMethod(mr) == ClassRef.Object }
        .foldLeft(orig.duplicate1(el)) {
          case (self, mr) =>
            val cr = self.resolveVirtualMethod(mr)
            el.log(s"Local instance inlining: $mr")
            val inlined =
              el.enterMethod(cr, mr) { el => inline(self.dataflow(cr, mr), el) }
            self.addMethod(mr, inlined)
        }
    }

    private[this] def inline(df: DataFlow, el: EventLogger): MethodBody = {
      import Bytecode._
      // TODO: check all required method is inlinable
      val inlinables: Map[(Bytecode.Label, DataPort.Out), (Instance.New[_ <: AnyRef], Map[(ClassRef, FieldRef), Int])] =
        // TODO: check used method only in ni.escaped
        df.newInstances
          .filter { case ((l, p), ni) => !df.escaped(l, p) && !ni.escaped }
          .filter { case ((l, p), ni) =>
            df.useSites(l, p).forall { case (ul, ubc, ups) =>
              ups.forall { up => df.dataSource(ul, up).unambiguous }
            }
          }.map {
            case (v, d) =>
              v -> (d -> d.klass.instanceFieldAttributes.keys.zipWithIndex.toMap)
          }.toMap
      def toInlineForm(
        base: DataFlow,
        fieldMap: Map[(ClassRef, FieldRef), Int],
        inlined: Set[(ClassRef, MethodRef)] = Set()
      ): CodeFragment = {
        val retValIndex = df.maxLocals
        val fieldLocalOffset = if(df.body.isStatic) retValIndex else retValIndex + 1
        val localOffset = fieldLocalOffset + fieldMap.size
        val prepareArgs = CodeFragment.bytecode(
          base.body.descriptor.args.reverse.zipWithIndex.map { case (t, i) =>
            autoStore(t, i + localOffset)
          }: _*
        )
        val inlinableBody = base.body.codeFragment
          .rewrite_* {
            case (l,  bc @ getfield(cr, fr)) if base.mustThis(l, bc.objectref) =>
              val index = fieldLocalOffset + fieldMap(base.instance.resolveField(cr, fr) -> fr)
              CodeFragment.bytecode(
                autoLoad(fr.typeRef, index)
              )
            case (l, bc @ putfield(cr, fr)) if base.mustThis(l, bc.objectref) =>
              val index = fieldLocalOffset + fieldMap(base.instance.resolveField(cr, fr) -> fr)
              CodeFragment.bytecode(
                autoStore(fr.typeRef, index)
              )
            case (_, bc: LocalAccess) =>
              CodeFragment.bytecode(bc.rewriteLocalIndex(bc.localIndex + localOffset))
            case (l, bc: InvokeInstanceMethod) if base.mustThis(l, bc.objectref) =>
              val cr = bc.resolveMethod(base.instance)
              if (inlined.contains(cr -> bc.methodRef))
                throw new RuntimeException("recursive method not supported")
              val body = toInlineForm(
                base.instance.dataflow(cr, bc.methodRef),
                fieldMap,
                inlined + (cr -> bc.methodRef)
              )
              ???
            case (l, bc: XReturn) =>
              val saveRetVal =
                CodeFragment.bytecode(autoStore(df.body.descriptor.ret, retValIndex))
              // pop stack
              val gotoExit =
                CodeFragment.abstractJump(goto(), "exit")
              ???
          }
        val exit =
          (if(df.body.isStatic) {
            CodeFragment.empty
          } else {
            CodeFragment.bytecode(autoLoad(df.body.descriptor.ret, retValIndex))
          }).name("exit")
        (prepareArgs + inlinableBody + exit).complete()
      }

      def inlinable(l: Bytecode.Label, p: DataPort): Option[(Instance.New[_ <: AnyRef], Map[(ClassRef, FieldRef), Int])] =
        df.dataSource(l, p).single.collect { case DataSource.New(l, p) => inlinables.get(l -> p) }.flatten

      df.body.rewrite_* {
        case (l, bc @ new_(cr)) if inlinables.contains(l -> bc.objectref) =>
          // inline ctor
          val (newInstance, fieldMap) = inlinables(l -> bc.objectref)
          toInlineForm(newInstance.constructorDataFlow, fieldMap)
        case (l, bc: InvokeInstanceMethod) if inlinable(l, bc.objectref).nonEmpty =>
          val Some((newInstance, fieldMap)) = inlinable(l, bc.objectref)
          toInlineForm(newInstance.dataflow(bc.resolveMethod(newInstance), bc.methodRef), fieldMap)
        case (l, bc @ getfield(cr, fr)) if inlinable(l, bc.objectref).nonEmpty =>
          val Some((newInstance, fieldMap)) = inlinable(l, bc.objectref)
          CodeFragment.bytecode(pop(), autoLoad(fr.typeRef, fieldMap(cr -> fr)))
        case (l, bc @ putfield(cr, fr)) if inlinable(l, bc.objectref).nonEmpty =>
          val Some((newInstance, fieldMap)) = inlinable(l, bc.objectref)
          CodeFragment.bytecode(autoStore(fr.typeRef, fieldMap(cr -> fr)), pop())
      }
    }
  }

  // TODO: eliminate load-pop pair etc
  // TODO: check access rights and resolve members
}
