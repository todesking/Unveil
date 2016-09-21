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
    bytecode: Seq[Bytecode],
    jumpTargets: Map[JumpTarget, Bytecode.Label]
) {
  Predef.require(bytecode.nonEmpty)

  private[this] def require(cond: Boolean, detail: => String = ""): Unit =
    MethodBody.require(this, cond, detail)

  def asCodeFragment: CodeFragment =
    CodeFragment(bytecode, jumpTargets)

  def isStatic: Boolean = attribute.isStatic

  def makePrivate: MethodBody =
    copy(attribute = attribute.makePrivate)

  def makeNonFinal: MethodBody =
    copy(attribute = attribute.makeNonFinal)

  // TODO: Exception handler

  def methodReferences: Set[(ClassRef, MethodRef)] =
    bytecode.collect { case bc: Bytecode.HasMethodRef => (bc.classRef -> bc.methodRef) }.toSet

  def fieldReferences: Set[(ClassRef, FieldRef)] =
    bytecode.collect { case bc: Bytecode.HasFieldRef => (bc.classRef -> bc.fieldRef) }.toSet

  lazy val labelToBytecode: Map[Bytecode.Label, Bytecode] =
    bytecode.map { bc => bc.label -> bc }.toMap

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

  def rewrite_**(f: PartialFunction[Bytecode, Map[Bytecode.Label, CodeFragment]]): MethodBody = {
    val lifted = f.lift
    val allRewrites =
      bytecode.foldLeft(Map.empty[Bytecode.Label, CodeFragment]) {
        case (m, bc) =>
          lifted(bc).fold(m) { mm =>
            Algorithm.sharedNothingUnion(m, mm).fold {
              throw new TransformException(s"rewrite conflict")
            }(identity)
          }
      }
    allRewrites.foldLeft(this) { case (b, (l, bcs)) => b.replaceBytecode(l, bcs) }
  }

  def rewriteClassRef(from: ClassRef, to: ClassRef): MethodBody = {
    rewrite { case bc: Bytecode.HasClassRef if bc.classRef == from => bc.rewriteClassRef(to) }
  }

  def replaceBytecode(l: Bytecode.Label, newBc: Bytecode): MethodBody =
    replaceBytecode(l, CodeFragment.bytecode(newBc))

  def replaceBytecode(l: Bytecode.Label, cf: CodeFragment): MethodBody = {
    require(labelToBytecode.contains(l))
    require(cf.bytecode.nonEmpty)
    if (cf.bytecode.size == 1 && cf.bytecode.head.label == l) { // TODO: WHY????
      // cf.jumpTargets could safely ignored
      this
    } else {
      val newCf = cf.fresh()
      val bcs = newCf.bytecode
      val first = bcs.head
      val start = bytecode.indexWhere(_.label == l)
      assert(start >= 0)
      val newBcs = bytecode.patch(start, bcs, 1)
      val newJts = jumpTargets.map { case (jt, bcl) => if (bcl == l) (jt -> first.label) else (jt -> bcl) } ++ newCf.jumpTargets
      copy(bytecode = newBcs, jumpTargets = newJts)
    }
  }

  def pretty: String = {
    val lName = Bytecode.Label.namer("L", "")
    s"""${
      bytecode.map { bc =>
        val l = f"L${bc.label.innerId}%-5s "
        l + (bc match {
          case j: Bytecode.Jump =>
            s"${j} # L${jumpTargets(j.target).innerId}"
          case b: Bytecode.Branch =>
            s"${b} # L${jumpTargets(b.target).innerId}"
          case b =>
            b.pretty
        })
      }.mkString("\n")
    }
"""
  }

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

