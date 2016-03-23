package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

import java.lang.reflect.{ Method => JMethod, Field => JField, Modifier }

import com.todesking.scalapp.syntax._

case class PartialInstance(
  thisRef: ClassRef,
  fields: Map[(ClassRef, FieldRef), Field],
  methods: Map[(ClassRef, MethodRef), MethodBody]
)
// TODO: add query methods about types(isDoubleWord etc) for FrameUpdate
case class FrameItem(label: DataLabel.Out, data: Data, placedBy: Option[Bytecode.Label])

class AccessibleClassLoader(parent: ClassLoader) extends ClassLoader(parent) {
  def registerClass(name: String, bytes: Array[Byte]): Unit = {
    defineClass(name, bytes, 0, bytes.size)
  }
}

case class CodeFragment(bytecode: Seq[Bytecode], jumpTargets: Map[JumpTarget, Bytecode.Label] = Map.empty) {
  def prependBytecode(bcs: Seq[Bytecode]): CodeFragment =
    copy(bytecode = bcs ++ bytecode)
}
object CodeFragment {
  def bytecode(bcs: Bytecode*): CodeFragment =
    CodeFragment(bcs, Map.empty)
}

final class InstructionLabel private () extends AbstractLabel
object InstructionLabel {
  def fresh(): InstructionLabel = new InstructionLabel
}

final class JumpTarget private extends AbstractLabel
object JumpTarget extends AbstractLabel.AssignerProvider[JumpTarget] {
  def fresh(): JumpTarget = new JumpTarget
}

sealed abstract class DataLabel private (val name: String) extends AbstractLabel
object DataLabel extends AbstractLabel.NamerProvider[DataLabel] {
  final class In(name: String) extends DataLabel(name) {
    override def toString = s"DataLabel.In(${name})#${innerId}"
  }
  final class Out(name: String) extends DataLabel(name) {
    override def toString = s"DataLabel.Out(${name})#${innerId}"
  }

  def in(name: String) = new In(name)
  def out(name: String) = new Out(name)
}

final class Effect private extends AbstractLabel
object Effect extends AbstractLabel.NamerProvider[Effect] {
  def fresh() = new Effect
}

