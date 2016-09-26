package com.todesking.unveil

import java.lang.reflect.{ Constructor, Method => JMethod }
import scala.util.{ Try, Success, Failure }
import scala.collection.mutable

// TODO: Analyzer
object Analyze {
  // TODO: add classRef
  class SetterConstructor(
      val descriptor: MethodDescriptor,
      val superConstructor: Option[SetterConstructor],
      val constantAssigns0: Map[(ClassRef, FieldRef), Any],
      val argumentAssigns0: Map[(ClassRef, FieldRef), Int]
  ) {
    def methodRef: MethodRef = MethodRef.constructor(descriptor)
    def constantAssigns: Map[(ClassRef, FieldRef), Any] =
      superConstructor.map(_.constantAssigns).getOrElse(Map.empty) ++ constantAssigns0
    def argumentAssigns: Map[(ClassRef, FieldRef), Int] =
      superConstructor.map(_.argumentAssigns).getOrElse(Map.empty) ++ argumentAssigns0 // TODO: WRONG
    def toArguments(fields: Map[(ClassRef, FieldRef), Field]): Seq[Any] = {
      require(assignable(fields))
      descriptor.args.zipWithIndex map {
        case (t, i) =>
          argumentAssigns.find(_._2 == i).map {
            case (k, v) =>
              fields(k).data.concreteValue
          } getOrElse {
            t.defaultValue
          }
      }
    }

    def sameArgumentValues: Seq[Set[(ClassRef, FieldRef)]] =
      argumentAssigns.groupBy(_._2).map { case (i, vs) => vs.map(_._1).toSet }.toSeq

    def assignable(fields: Map[(ClassRef, FieldRef), Field]): Boolean = {
      fields forall {
        case ((cr, fr), f1) =>
          constantAssigns.get(cr -> fr).map { v2 =>
            isSameValue(fr.typeRef, f1.data.concreteValue, v2)
          } getOrElse {
            argumentAssigns.contains(cr -> fr) &&
              sameArgumentValues.forall { s =>
                !s.contains(cr -> fr) || sameArgumentValues.forall { s =>
                  fields
                    .toSeq
                    .filter { case (k, f) => s.contains(k) }
                    .sliding(2)
                    .forall {
                      case Seq(((cr1, fr1), f1), ((cr2, fr2), f2)) =>
                        isSameValue(fr1.typeRef, f1.data.concreteValue, f2.data.concreteValue)
                      case Seq(_) => true
                    }
                }
              }
          }
      }
    }
    private[this] def isSameValue(t: TypeRef, v1: Any, v2: Any): Boolean =
      t match {
        case t: TypeRef.Primitive => v1 == v2
        case _ => ???
      }
    override def toString =
      s"""SetterConstructor(${descriptor}, ${constantAssigns}, ${argumentAssigns})"""

    def pretty = s"""SetterConstructor
    |  Values from argument:
    |${argumentAssigns.map { case ((cr, fr), i) => f"    ${i}%5d => ${cr}.${fr}" }.mkString("\n")}
    |  Values from constant:
    |${constantAssigns.map { case ((cr, fr), v) => f"    ${v}%5s => ${cr}.${fr}" }.mkString("\n")}
    """.stripMargin
  }
  object SetterConstructor {
    def from(self: Instance[_ <: AnyRef], ctorClass: ClassRef, body: MethodBody): Try[SetterConstructor] = {
      def makeError(msg: String) =
        new MethodAnalyzeException(ctorClass, MethodRef.constructor(body.descriptor), msg)
      val df = body.dataflow(self)
      import Bytecode._
      try {
        var superConstructor: Option[SetterConstructor] = None
        val constAssigns = mutable.HashMap.empty[(ClassRef, FieldRef), Any]
        val argAssigns = mutable.HashMap.empty[(ClassRef, FieldRef), Int]
        body.bytecode.foreach {
          case (label, bc) if df.possibleReturns(label).isEmpty =>
          // ignore error path
          case (label, bc: Shuffle) =>
          case (label, bc: Jump) =>
          case (label, bc: Return) =>
          case (label, bc: ConstX) =>
          case (label, bc: Branch) if df.possibleReturns(body.jumpTargets(label -> bc.jumpTarget)).isEmpty || df.possibleReturns(df.fallThroughs(label)).isEmpty =>
          // OK if one of jumps exit by throw
          case (label, bc @ invokespecial(classRef, methodRef)) if df.isThis(label, bc.objectref).getOrElse(false) && methodRef.isInit =>
            // super ctor invocation
            if (superConstructor.nonEmpty)
              throw makeError(s"Another constructor called twice in ${ctorClass}.<init>${body.descriptor}")
            superConstructor =
              SetterConstructor.from(self, classRef, self.methodBody(classRef, methodRef)).map(Some(_)).get
          case (label, bc @ putfield(classRef, fieldRef)) if df.isThis(label, bc.objectref).getOrElse(false) =>
            df.dataValue(label, bc.value).value.map { v =>
              // value from constant
              constAssigns += (classRef -> fieldRef) -> v
            } getOrElse {
              df.argNum(label, bc.value).fold {
                throw makeError(s"putfield non-argument/constant value(${df.dataValue(label, bc.value)}) is not acceptable: ${bc}")
              } { i =>
                argAssigns += (classRef -> fieldRef) -> i
              }
            }
          case bc =>
            throw makeError(s"Bytecode ${bc} is not acceptable in setter constructor")
        }
        Success(new SetterConstructor(body.descriptor, superConstructor, constAssigns.toMap, argAssigns.toMap))
      } catch {
        case e: UnveilException => Failure(e)
      }
    }
  }

  def setterConstructorsTry(self: Instance[_ <: AnyRef], klass: Class[_]): Seq[Try[SetterConstructor]] = {
    val classRef = ClassRef.of(klass)
    klass
      .getDeclaredConstructors
      .filterNot { c => MethodAttribute.Private.enabledIn(c.getModifiers) }
      .map { c => MethodRef.from(c) }
      .map { mr => SetterConstructor.from(self, classRef, self.methodBody(classRef, mr)) }
  }

  def setterConstructors(self: Instance[_ <: AnyRef], klass: Class[_]): Seq[SetterConstructor] =
    setterConstructorsTry(self, klass)
      .collect { case Success(sc) => sc }

  def findSetterConstructor[A](
    self: Instance[_ <: AnyRef],
    klass: Class[A],
    fields: Map[(ClassRef, FieldRef), Field]
  ): Option[SetterConstructor] = {
    setterConstructors(self, klass)
      .filter { _.assignable(fields) }
      .headOption
  }
}

