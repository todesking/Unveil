package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._

// TODO: posh(DataLabel.Out, Data) etc // TODO: WHAT is posh???
case class FrameUpdate(
    label: Bytecode.Label,
    bytecode: Bytecode,
    newFrame: Frame,
    frameItems: Map[(Bytecode.Label, DataPort), FrameItem]
) {
  def this(label: Bytecode.Label, bytecode: Bytecode, frame: Frame) =
    this(
      label,
      bytecode,
      frame,
      Map.empty
    )

  lazy val dataValues: Map[(Bytecode.Label, DataPort), Data] =
    frameItems.mapValues(_.data)

  lazy val dataSources: Map[(Bytecode.Label, DataPort), DataSource] =
    frameItems.mapValues(_.source)

  private[this] def fail(msg: String): RuntimeException =
    new RuntimeException(s"Analysis failed at ${label.format("L%d")} ${bytecode}: ${msg}")

  private[this] def requireSingleLocal(n: Int): Unit = {
    if (!newFrame.locals.contains(n)) throw fail(s"Local $n not defined")
    requireSingleWord(newFrame.locals(n))
  }

  private[this] def requireSecondWord(fi: FrameItem): Unit =
    if (fi.data.typeRef != TypeRef.SecondWord)
      throw fail(s"second word value expected but ${fi}")

  private[this] def requireSingleWord(fi: FrameItem): Unit =
    if (fi.data.typeRef.isDoubleWord || fi.data.typeRef == TypeRef.SecondWord || fi.data.typeRef == TypeRef.Undefined)
      throw fail(s"single word value expected but ${fi}")

  private[this] def requireDoubleWord(fi: FrameItem): Unit =
    if (!fi.data.typeRef.isDoubleWord || fi.data.typeRef == TypeRef.SecondWord || fi.data.typeRef == TypeRef.Undefined)
      throw fail(s"double word value expected but ${fi}")

  private[this] def requireStackTopType(f: Frame, t: TypeRef): Unit = t match {
    case t: TypeRef.DoubleWord =>
      if (f.stack.size < 2) throw fail("double word expected but stack too short")
      if (!t.isAssignableFrom(f.stack(0).data.typeRef)) throw fail(s"$t expected but ${f.stack(0).data.typeRef}")
      requireSecondWord(f.stack(1))
    case t: TypeRef.SingleWord =>
      if (f.stack.size < 1) throw fail("single word expected but stack too short")
      if (!t.isAssignableFrom(f.stack(0).data.typeRef)) throw fail(s"$t expected but ${f.stack(0).data.typeRef}")
  }

  private[this] def makeSecondWord(fi: FrameItem): FrameItem =
    FrameItem(fi.source, fi.data.secondWordData)

  def pop(t: TypeRef): FrameUpdate =
    if (t.isDoubleWord) pop2()
    else pop1()

  def pop(t: TypeRef, in: DataPort.In): FrameUpdate =
    if (t.isDoubleWord) pop2(in)
    else pop1(in)

  def pop1(): FrameUpdate = {
    requireSingleWord(newFrame.stackTop)
    copy(
      newFrame = newFrame.copy(stack = newFrame.stack.drop(1))
    )
  }

  def pop1(in: DataPort.In): FrameUpdate = {
    requireSingleWord(newFrame.stackTop)
    val x = newFrame.stackTop
    pop0(in, x, newFrame.stack.drop(1))
  }

  def pop2(): FrameUpdate = {
    // TODO[BUG]: pop2 can pop 2 single word
    requireDoubleWord(newFrame.stack(0))
    requireSecondWord(newFrame.stack(1))
    copy(newFrame = newFrame.copy(stack = newFrame.stack.drop(2)))
  }

  def pop2(in: DataPort.In): FrameUpdate = {
    // TODO[BUG]: pop2 can pop 2 single word
    requireDoubleWord(newFrame.stack(0))
    requireSecondWord(newFrame.stack(1))
    val x = newFrame.stack(0)
    pop0(in, x, newFrame.stack.drop(2))
  }

  private[this] def pop0(in: DataPort.In, fi: FrameItem, stack: List[FrameItem]): FrameUpdate =
    FrameUpdate(
      label,
      bytecode,
      newFrame.copy(stack = stack),
      frameItems + ((label -> in) -> fi)
    )

  def push(p: Option[DataPort], d: FrameItem): FrameUpdate =
    if (d.data.typeRef.isDoubleWord) push2(p, d)
    else push1(p, d)

  def push1(p: Option[DataPort], d: FrameItem): FrameUpdate = {
    requireSingleWord(d)
    push0(p, d, d :: newFrame.stack)
  }

  def push2(p: Option[DataPort], d: FrameItem): FrameUpdate = {
    requireDoubleWord(d)
    push0(p, d, d :: makeSecondWord(d) :: newFrame.stack)
  }

  private[this] def push0(p: Option[DataPort], fi: FrameItem, stack: List[FrameItem]): FrameUpdate =
    FrameUpdate(
      label,
      bytecode,
      newFrame.copy(stack = stack),
      p.fold(frameItems) { p => frameItems + ((label -> p) -> fi) }
    )

  def setLocal(n: Int, data: FrameItem): FrameUpdate = {
    val locals =
      if (data.data.typeRef.isDoubleWord)
        newFrame.locals.updated(n, data).updated(n + 1, makeSecondWord(data))
      else
        newFrame.locals.updated(n, data)
    FrameUpdate(
      label,
      bytecode,
      newFrame.copy(locals = newFrame.locals.updated(n, data)),
      frameItems
    )
  }

  private[this] def local1(n: Int): FrameItem = {
    requireSingleLocal(n)
    newFrame.locals(n)
  }

  private[this] def local2(n: Int): FrameItem = {
    requireDoubleWord(newFrame.locals(n))
    requireSecondWord(newFrame.locals(n + 1))
    newFrame.locals(n)
  }

  def load1(n: Int): FrameUpdate = push1(None, local1(n))
  def load2(n: Int): FrameUpdate = push2(None, local2(n))

  def store1(tpe: TypeRef.SingleWord, n: Int): FrameUpdate = {
    requireStackTopType(newFrame, tpe)
    setLocal(n, newFrame.stackTop)
      .pop1()
  }

  def store2(tpe: TypeRef.DoubleWord, n: Int): FrameUpdate = {
    requireStackTopType(newFrame, tpe)
    setLocal(n, newFrame.stackTop)
      .setLocal(n + 1, makeSecondWord(newFrame.stackTop))
      .pop2()
  }

  def ret(retval: DataPort.In): FrameUpdate = {
    val fi =
      if (newFrame.stackTop.data.typeRef == TypeRef.SecondWord) {
        requireDoubleWord(newFrame.stack(1))
        newFrame.stack(1)
      } else {
        newFrame.stackTop
      }
    FrameUpdate(
      label,
      bytecode,
      Frame(Map.empty, List.empty),
      frameItems + ((label -> retval) -> fi)
    )
  }

  def athrow(objectref: DataPort.In): FrameUpdate = {
    requireSingleWord(newFrame.stackTop)
    FrameUpdate(
      label,
      bytecode,
      newFrame.copy(stack = newFrame.stack.take(1)),
      frameItems
    )
  }
}
