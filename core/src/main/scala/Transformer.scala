package com.todesking.unveil

import scala.language.existentials

import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

trait Transformer { self =>
  def name: String
  def params: Map[String, String] = Map.empty

  def apply[A <: AnyRef](orig: Instance[A], el: EventLogger): Try[Instance.Duplicate[A]] =
    try {
      el.enterTransformer(this, orig) { el => Success(apply0(orig, el)) }
    } catch {
      case e: UnveilException =>
        el.fail(e)
        Failure(e)
    }
  protected[this] def apply0[A <: AnyRef](orig: Instance[A], el: EventLogger): Instance.Duplicate[A]

  def andThen(next: Transformer): Transformer =
    new Transformer {
      override def name = s"${self.name} >>> ${next.name}"
      override def apply[A <: AnyRef](orig: Instance[A], el: EventLogger): Try[Instance.Duplicate[A]] = {
        el.enterTransformer(this, orig) { el =>
          self.apply(orig, el).flatMap { i2 => next.apply(i2, el) }
        }
      }
      override def apply0[A <: AnyRef](orig: Instance[A], el: EventLogger) = throw new AssertionError()
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
    override def apply0[A <: AnyRef](instance: Instance[A], el: EventLogger): Instance.Duplicate[A] = {
      val dupInstance = instance.duplicate1(el)
      fuse(
        "",
        dupInstance,
        dupInstance
          .rewritableVirtualMethods
          .keySet
          .filterNot { mr => dupInstance.resolveVirtualMethod(mr) == ClassRef.Object }
          .map { mr => dupInstance.resolveVirtualMethod(mr) -> mr } ++ (
            dupInstance
            .thisMethods
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
              case Data.Reference(t, fieldInstance) if !fieldInstance.fields.forall(_._2.attribute.isFinal) =>
                el.log("Pass: This field is instance-stateful")
                self
              case Data.Reference(t, fieldInstance) =>
                // TODO: log
                val usedMethods = fieldInstance.extendMethods(
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
                          case bc: Bytecode.InvokeInstanceMethod if df.mustThis(bc.objectref) =>
                            Bytecode.invokespecial(self.thisRef, methodRenaming(bc.classRef, bc.methodRef))
                          case bc: Bytecode.InstanceFieldAccess if df.mustThis(bc.objectref) =>
                            bc.rewriteFieldRef(self.thisRef, fieldRenaming(bc.classRef, bc.fieldRef))
                        }.makePrivate
                    }
                val rewrittenMethods =
                  methods
                    .map {
                      case (cr, mr) =>
                        val df = self.dataflow(cr, mr)
                        import Bytecode._
                        val memberAccessOnly =
                          df.body.bytecode
                            .filter { bc => bc.inputs.exists { i => df.mustInstance(i, fieldInstance) } }
                            .forall {
                              case bc @ getfield(_, _) => true
                              case bc: InvokeInstanceMethod if !bc.args.exists { a => df.mustInstance(a, fieldInstance) } => true
                              case bc => false
                            }
                        if (!memberAccessOnly) {
                          el.log(s"[SKIP] the field is leaked in method $mr")
                          mr -> df.body
                        } else {
                          // TODO: use df.mustFieldRef instead of df.mustInstance when rewriting
                          mr -> df.body.rewrite {
                            case bc @ getfield(cr1, fr1) if df.mustThis(bc.objectref) && self.resolveField(cr1, fr1) == cr && fr1 == fr =>
                              nop()
                            case bc: InvokeInstanceMethod if df.mustInstance(bc.objectref, fieldInstance) =>
                              methodRenaming.get(fieldInstance.resolveVirtualMethod(bc.methodRef) -> bc.methodRef).fold {
                                throw new AssertionError(s"Can't find renamed method for ${bc.classRef}.${bc.methodRef}")
                              } { mr =>
                                invokespecial(self.thisRef, mr)
                              }
                            case bc: InstanceFieldAccess if df.mustInstance(bc.objectref, fieldInstance) =>
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
    override def apply0[A <: AnyRef](orig: Instance[A], el: EventLogger): Instance.Duplicate[A] = {
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
        case bc: InvokeInstanceMethod if df.mustThis(bc.objectref) =>
          el.section(s"Inline invocation of ${bc.classRef}.${bc.methodRef}") { el =>
            val mr = bc.methodRef
            val cr =
              bc match {
                case invokespecial(cr, mr) =>
                  // TODO[BUG]: resolve special
                  cr
                case invokevirtual(cr, mr) =>
                  df.self.instance.resolveVirtualMethod(mr)
                case invokeinterface(cr, mr, _) =>
                  df.self.instance.resolveVirtualMethod(mr)
              }
            val calleeDf =
              if (ignore.contains(cr -> mr)) df.self.instance.dataflow(cr, mr)
              else inline(df.self.instance.dataflow(cr, mr), ignore + (cr -> mr), el).dataflow(df.self.instance)

            // TODO[BUG]: if(calleeDf.localModified(0)) ...
            val argOffset = if (calleeDf.body.isStatic) localOffset else localOffset + 1
            // TODO: support exception
            val cf =
              calleeDf.body.rewrite_* {
                case bc: LocalAccess =>
                  CodeFragment.bytecode(bc.rewriteLocalIndex(bc.localIndex + localOffset))
                case bc: XReturn =>
                  val resultLocal = localOffset + calleeDf.maxLocals
                  CodeFragment(
                    Seq(store(bc.returnType, resultLocal)) ++ (
                      calleeDf.beforeFrames(bc.label).stack.drop(bc.returnType.wordSize).map {
                        case FrameItem(l, d, _) =>
                          autoPop(d.typeRef)
                      }
                    ) ++ Seq(
                        load(bc.returnType, resultLocal)
                      )
                  )
                case bc: VoidReturn =>
                  CodeFragment(
                    calleeDf.beforeFrames(bc.label).stack.map {
                      case FrameItem(l, d, _) =>
                        autoPop(d.typeRef)
                    }
                  )
              }.asCodeFragment
                .prependBytecode(
                  mr.descriptor.args.reverse.zipWithIndex.map { case (t, i) => store(t, i + argOffset) } ++
                    (if (calleeDf.body.isStatic) Seq.empty else Seq(astore(localOffset)))
                )
            localOffset += calleeDf.maxLocals + 1 // TODO: inefficient if static method
            cf
          }
      }
    }
  }

  // TODO: eliminate load-pop pair etc
  // TODO: check access rights and resolve members
}
