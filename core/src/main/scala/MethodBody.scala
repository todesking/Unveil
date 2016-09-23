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

  def jumpTargets: Map[JumpTarget, Bytecode.Label] =
    codeFragment.jumpTargets

  def bytecode: Seq[Bytecode] =
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

  def rewrite(f: PartialFunction[Bytecode, Bytecode]): MethodBody = {
    val lifted = f.lift
    rewrite_* {
      case bc if f.isDefinedAt(bc) => CodeFragment.bytecode(f(bc))
    }
  }

  def rewrite_*(f: PartialFunction[Bytecode, CodeFragment]): MethodBody = {
    rewrite_** {
      case bc if f.isDefinedAt(bc) => Map(bc.label -> f(bc))
    }
  }

  def rewrite_**(
    f: PartialFunction[Bytecode, Map[Bytecode.Label, CodeFragment]]
  ): MethodBody =
    copy(codeFragment = codeFragment.rewrite_**(f))

  def rewriteClassRef(from: ClassRef, to: ClassRef): MethodBody = {
    rewrite { case bc: Bytecode.HasClassRef if bc.classRef == from => bc.rewriteClassRef(to) }
  }

  def replaceBytecode(l: Bytecode.Label, newBc: Bytecode): MethodBody =
    replaceBytecode(l, CodeFragment.bytecode(newBc))

  def replaceBytecode(l: Bytecode.Label, cf: CodeFragment): MethodBody =
    copy(codeFragment = codeFragment.replaceBytecode(l, cf))

  def pretty: String =
    s"""$descriptor [$attribute]
    ${codeFragment.pretty}"""

  def dataflow(self: Instance[_ <: AnyRef]): DataFlow =
    new DataFlow(this, Data.Reference(self.thisRef.toTypeRef, self))
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

