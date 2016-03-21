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
}
object Transformer {
  def newEventLogger(): EventLogger =
    new EventLogger

  // TODO: lowerMembers
  object lowerPrivateFields extends Transformer {
    override def name = "lowerPrivateFields"
    override def apply0[A <: AnyRef](orig: Instance[A], el: EventLogger): Instance.Duplicate[A] = {
      val dupInstance = orig.duplicate1
      // TODO: support invokespecial methods
      val entryMethods =
        dupInstance.virtualMethods
          .filter {
            case (mr, a) =>
              dupInstance.resolveVirtualMethod(mr) != ClassRef.Object && !a.isFinal
          }.map {
            case (mr, a) =>
              mr -> dupInstance.dataflow(mr)
          }.filter {
            case (mr, df) =>
              // invokespecial need special handling
              df.body.bytecode.forall {
                case bc @ Bytecode.invokespecial(_, _) if df.mustInstance(bc.objectref, dupInstance) => false
                case _ => true
              }
          }.filter {
            case (mr, df) =>
              df.usedFieldsOf(dupInstance).forall {
                case (cr, mr) =>
                  val attr = dupInstance.fields(cr -> mr).attribute
                  cr != dupInstance.thisRef && attr.isPrivate
              }
          }
      el.logMethods("lowered methods", entryMethods.keys)
      val copyFields: Map[(ClassRef, FieldRef), (FieldRef, Field)] =
        entryMethods.flatMap {
          case (mr, df) =>
            df.usedFieldsOf(dupInstance).filter {
              case (cr, fr) => cr != dupInstance.thisRef && dupInstance.fields(cr -> fr).attribute.isPrivateFinal
            }
        }.map {
          case (cr, fr) =>
            val f = dupInstance.fields(cr -> fr)
            (cr -> fr) -> (fr.anotherUniqueName(fr.name) -> f)
        }
      el.logCFields("lowered fields", copyFields.keys)
      val overridenMethods =
        entryMethods.map {
          case (mr, df) =>
            import Bytecode._
            mr -> df.body.rewrite {
              case bc: InstanceFieldAccess if df.mustInstance(bc.objectref, dupInstance) && copyFields.contains(bc.classRef -> bc.fieldRef) =>
                val (fr, _) = copyFields(bc.classRef -> bc.fieldRef)
                bc.rewriteClassRef(dupInstance.thisRef).rewriteFieldRef(fr)
            }
        }
      dupInstance
        .addFields(copyFields.map { case (_, (newFr, f)) => newFr -> f })
        .addMethods(overridenMethods)
    }
  }

  // TODO: support instance-stateful fields(need leakage detection)
  // TODO: support mutable fields(if fref eq original then optimized else original)
  object fieldFusion extends Transformer {
    override def name = s"fieldFusion"
    override def apply0[A <: AnyRef](instance: Instance[A], el: EventLogger): Instance.Duplicate[A] = {
      val dupInstance = instance.duplicate1
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
                            bc.rewriteMethodRef(self.thisRef, methodRenaming(bc.classRef, bc.methodRef))
                          case bc: Bytecode.InstanceFieldAccess if df.mustThis(bc.objectref) =>
                            bc.rewriteFieldRef(self.thisRef, fieldRenaming(bc.classRef, bc.fieldRef))
                        }
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
                            case bc @ getfield(cr1, fr1) if df.mustThis(bc.objectref) && cr1 == cr && fr1 == fr =>
                              nop()
                            case bc: InvokeInstanceMethod if df.mustInstance(bc.objectref, fieldInstance) =>
                              methodRenaming.get(bc.classRef, bc.methodRef).fold(bc) { mr =>
                                bc.rewriteMethodRef(self.thisRef, mr)
                              }
                            case bc: InstanceFieldAccess if df.mustInstance(bc.objectref, fieldInstance) =>
                              fieldRenaming.get(bc.classRef, bc.fieldRef).fold(bc) {
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
        .foldLeft(orig.duplicate1) {
          case (self, mr) =>
            val cr = orig.resolveVirtualMethod(mr)
            el.log(s"Inlining $mr")
            val inlined =
              el.enterMethod(cr, mr) { el => inline(orig.dataflow(cr, mr), Set(cr -> mr)) }
            self.addMethod(mr, inlined)
        }
    }

    private[this] def inline(df: DataFlow, ignore: Set[(ClassRef, MethodRef)]): MethodBody = {
      // TODO: if(df.localModified(0)) df.body
      var localOffset = df.maxLocals
      import Bytecode._
      df.body.rewrite_* {
        case bc: InvokeInstanceMethod if df.mustThis(bc.objectref) =>
          val mr = bc.methodRef
          val cr =
            bc match {
              case invokespecial(cr, mr) =>
                // TODO[BUG]: resolve special
                cr
              case invokevirtual(cr, mr) =>
                df.self.instance.resolveVirtualMethod(mr)
            }
          val calleeDf = df.self.instance.dataflow(cr, mr)

          // TODO: if(calleeDf.localModified(0)) ...
          val argOffset = if (calleeDf.body.isStatic) localOffset else localOffset + 1
          val cf =
            calleeDf.body.rewrite_* {
              case bc: LocalAccess =>
                CodeFragment.bytecode(bc.rewriteLocalIndex(bc.localIndex + localOffset))
              case bc: XReturn =>
                val resultLocal = localOffset + calleeDf.maxLocals
                CodeFragment(
                  Seq(store(bc.returnType, resultLocal)) ++ (
                    calleeDf.beforeFrames(bc.label).stack.tail.map {
                      case FrameItem(l, d, _) =>
                        autoPop(d.typeRef)
                    }
                  ) ++ Seq(
                      load(bc.returnType, resultLocal)
                    )
                )
              case bc: VoidReturn =>
                CodeFragment(
                  calleeDf.beforeFrames(bc.label).stack.tail.map {
                    case FrameItem(l, d, _) =>
                      autoPop(d.typeRef)
                  }
                )
            }.asCodeFragment
              .prependBytecode(
                mr.descriptor.args.reverse.zipWithIndex.map { case (t, i) => store(t, i + argOffset) } ++
                  (if (calleeDf.body.isStatic) Seq.empty else Seq(pop()))
              )
          localOffset += calleeDf.maxLocals + 1 // TODO: inefficient if static method
          cf
      }
    }
  }

  // TODO: eliminate load-pop pair etc
}
