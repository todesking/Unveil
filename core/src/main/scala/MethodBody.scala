package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod, Constructor => JConstructor }

import com.todesking.scalapp.syntax._

case class MethodBody(
    descriptor: MethodDescriptor,
    attribute: MethodAttribute,
    codeFragment: CodeFragment
) {
  private[this] def require(cond: Boolean, detail: => String = ""): Unit =
    MethodBody.require(this, cond, detail)

  def bytecodeFromLabel(l: Bytecode.Label): Bytecode =
    codeFragment.bytecodeFromLabel(l)

  def jumpTargets: Map[(Bytecode.Label, JumpTarget), Bytecode.Label] =
    codeFragment.jumpTargets

  def bytecode: Seq[(Bytecode.Label, Bytecode)] =
    codeFragment.bytecode

  def isStatic: Boolean = attribute.isStatic

  def makePrivate: MethodBody =
    copy(attribute = attribute.makePrivate)

  def makeNonFinal: MethodBody =
    copy(attribute = attribute.makeNonFinal)

  // TODO: Exception handler

  def methodReferences: Set[(ClassRef, MethodRef)] =
    codeFragment.methodReferences

  def fieldReferences: Set[(ClassRef, FieldRef)] =
    codeFragment.fieldReferences

  def labelToBytecode: Map[Bytecode.Label, Bytecode] =
    codeFragment.labelToBytecode

  def rewrite(f: PartialFunction[(Bytecode.Label, Bytecode), Bytecode]): MethodBody = {
    val lifted = f.lift
    rewrite_* {
      case x @ (label, bc) if f.isDefinedAt(x) => CodeFragment.bytecode(f(x))
    }
  }

  def rewrite_*(f: PartialFunction[(Bytecode.Label, Bytecode), CodeFragment]): MethodBody = {
    rewrite_** {
      case x @ (label, bc) if f.isDefinedAt(x) => Map(label -> f(x))
    }
  }

  def rewrite_**(
    f: PartialFunction[(Bytecode.Label, Bytecode), Map[Bytecode.Label, CodeFragment]]
  ): MethodBody =
    copy(codeFragment = codeFragment.rewrite_**(f))

  def rewriteClassRef(from: ClassRef, to: ClassRef): MethodBody = {
    rewrite { case (label, bc: Bytecode.HasClassRef) if bc.classRef == from => bc.rewriteClassRef(to) }
  }

  def pretty: String =
    s"""$descriptor [$attribute]
${codeFragment.pretty}"""

  // TODO: change to dataflow(klass: Klass, values: Map[(CR, FR), Any] = Map.empty)
  def dataflow(self: Instance[_ <: AnyRef]): DataFlow =
    new DataFlow(this, self.klass, self.fieldValues)

  def dataflow(klass: Klass): DataFlow = new DataFlow(this, klass, Map())
}

object MethodBody {
  def parse(m: JMethod): MethodBody =
    Javassist.decompile(m).getOrElse { throw new MethodAnalyzeException(ClassRef.of(m.getDeclaringClass), MethodRef.from(m), "CA not found") }

  def parse(m: JConstructor[_]): MethodBody =
    Javassist.decompile(m).getOrElse { throw new MethodAnalyzeException(ClassRef.of(m.getDeclaringClass), MethodRef.from(m), "CA not found") }

  def require(body: MethodBody, cond: Boolean, detailMsg: => String): Unit =
    if (!cond)
      throw new UnveilBugException("BUG", null) {
        override val detail = body.pretty + "\n\n" + detailMsg
      }
}

