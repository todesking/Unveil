package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._

// TODO: posh(DataLabel.Out, Data) etc
case class FrameUpdate(
    bytecode: Bytecode,
    newFrame: Frame,
    binding: Map[DataLabel.In, DataLabel.Out],
    effectDependencies: Map[Bytecode.Label, Effect],
    dataValues: Map[DataLabel, FrameItem]
) {
  def this(bytecode: Bytecode, frame: Frame) =
    this(
      bytecode,
      frame.copy(effect = bytecode.effect getOrElse frame.effect),
      Map.empty,
      bytecode.effect.map { _ => bytecode.label -> frame.effect }.toMap,
      Map.empty
    )

  private[this] def requireSecondWord(fd: FrameItem): Unit =
    if (fd.data.typeRef != TypeRef.SecondWord)
      throw new RuntimeException(s"second word value expected but ${fd}")

  private[this] def requireSingleWord(fd: FrameItem): Unit =
    if (fd.data.typeRef.isDoubleWord || fd.data.typeRef == TypeRef.SecondWord || fd.data.typeRef == TypeRef.Undefined)
      throw new RuntimeException(s"single word value expected but ${fd}")

  private[this] def requireDoubleWord(fd: FrameItem): Unit =
    if (!fd.data.typeRef.isDoubleWord || fd.data.typeRef == TypeRef.SecondWord || fd.data.typeRef == TypeRef.Undefined)
      throw new RuntimeException(s"double word value expected but ${fd}")

  private[this] def makeSecondWord(fd: FrameItem): FrameItem =
    FrameItem(DataLabel.out(s"second word of ${fd.label.name}"), fd.data.secondWordData, fd.placedBy)

  def pop(t: TypeRef): FrameUpdate =
    if (t.isDoubleWord) pop2()
    else pop1()

  def pop(t: TypeRef, in: DataLabel.In): FrameUpdate =
    if (t.isDoubleWord) pop2(in)
    else pop1(in)

  def pop1(): FrameUpdate = {
    requireSingleWord(newFrame.stackTop)
    copy(
      newFrame = newFrame.copy(stack = newFrame.stack.drop(1))
    )
  }

  def pop1(in: DataLabel.In): FrameUpdate = {
    requireSingleWord(newFrame.stackTop)
    val x = newFrame.stackTop
    pop0(in, x, newFrame.stack.drop(1))
  }

  def pop2(): FrameUpdate = {
    requireSecondWord(newFrame.stack(0))
    requireDoubleWord(newFrame.stack(1))
    copy(newFrame = newFrame.copy(stack = newFrame.stack.drop(2)))
  }

  def pop2(in: DataLabel.In): FrameUpdate = {
    requireSecondWord(newFrame.stack(0))
    requireDoubleWord(newFrame.stack(1))
    val x = newFrame.stack(1)
    pop0(in, x, newFrame.stack.drop(2))
  }

  private[this] def pop0(in: DataLabel.In, fi: FrameItem, stack: List[FrameItem]): FrameUpdate =
    FrameUpdate(
      bytecode,
      newFrame.copy(stack = stack),
      binding + (in -> fi.label),
      effectDependencies,
      dataValues + (in -> fi)
    )

  def push(d: FrameItem): FrameUpdate =
    if (d.data.typeRef.isDoubleWord) push2(d)
    else push1(d)

  def push1(d: FrameItem): FrameUpdate = {
    requireSingleWord(d)
    push0(d, d :: newFrame.stack)
  }

  def push2(d: FrameItem): FrameUpdate = {
    requireDoubleWord(d)
    push0(d, makeSecondWord(d) :: d :: newFrame.stack)
  }

  private[this] def push0(d: FrameItem, stack: List[FrameItem]): FrameUpdate =
    FrameUpdate(
      bytecode,
      newFrame.copy(stack = stack),
      binding,
      effectDependencies,
      dataValues + (d.label -> d)
    )

  def setLocal(n: Int, data: FrameItem): FrameUpdate = {
    val locals =
      if (data.data.typeRef.isDoubleWord)
        newFrame.locals.updated(n, data).updated(n + 1, makeSecondWord(data))
      else
        newFrame.locals.updated(n, data)
    FrameUpdate(
      bytecode,
      newFrame.copy(locals = newFrame.locals.updated(n, data)),
      binding,
      effectDependencies,
      dataValues
    )
  }

  private[this] def local1(n: Int): FrameItem = {
    requireSingleWord(newFrame.locals(n))
    newFrame.locals(n)
  }

  private[this] def local2(n: Int): FrameItem = {
    requireDoubleWord(newFrame.locals(n))
    requireSecondWord(newFrame.locals(n + 1))
    newFrame.locals(n)
  }

  def load1(n: Int): FrameUpdate = push1(local1(n).copy(placedBy = Some(bytecode.label)))
  def load2(n: Int): FrameUpdate = push2(local2(n).copy(placedBy = Some(bytecode.label)))

  def store1(n: Int): FrameUpdate = {
    requireSingleWord(newFrame.stackTop)
    setLocal(n, newFrame.stackTop)
  }

  def ret(retval: DataLabel.In): FrameUpdate = {
    val d =
      if (newFrame.stackTop.data.typeRef == TypeRef.SecondWord) newFrame.stack(1)
      else newFrame.stackTop
    FrameUpdate(
      bytecode,
      Frame(Map.empty, List.empty, newFrame.effect),
      binding + (retval -> d.label),
      effectDependencies,
      dataValues + (retval -> d)
    )
  }

  def athrow(objectref: DataLabel.In): FrameUpdate = {
    requireSingleWord(newFrame.stackTop)
    FrameUpdate(
      bytecode,
      newFrame.copy(stack = newFrame.stack.take(1)),
      binding + (objectref -> newFrame.stack.head.label),
      effectDependencies,
      dataValues
    )
  }
}