package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable

import java.lang.reflect.{ Method => JMethod }

import com.todesking.scalapp.syntax._
sealed abstract class Bytecode {
  type Self <: Bytecode
  protected final def self: Self = this.asInstanceOf[Self] // :(

  def inputs: Seq[DataPort.In]
  def output: Option[DataPort.Out]
  def nextFrame(label: Bytecode.Label, frame: Frame): FrameUpdate
  def pretty: String = toString

  protected def update(label: Bytecode.Label, frame: Frame): FrameUpdate =
    new FrameUpdate(label, this, frame)
}
object Bytecode {
  case class Label(index: Int) {
    def format(f: String): String =
      f.format(index)

    def offset(n: Int): Label =
      Label(index + n)

    override def toString = s"L$index"
  }

  def autoLoad(t: TypeRef, n: Int): Bytecode =
    t match {
      case TypeRef.Int => iload(n)
      case TypeRef.Double => dload(n)
      case TypeRef.Float => fload(n)
      case TypeRef.Long => lload(n)
      case TypeRef.Reference(_) => aload(n)
      case unk =>
        throw new IllegalArgumentException(s"Unsupported load instruction for ${unk}")
    }

  def autoStore(t: TypeRef, n: Int): Bytecode =
    t match {
      case TypeRef.Int => istore(n)
      case TypeRef.Double => dstore(n)
      case TypeRef.Reference(cr) => astore(n)
      case unk =>
        throw new IllegalArgumentException(s"Unsupported store instruction for ${unk}")
    }

  def autoPop(t: TypeRef): Bytecode =
    if (t.isDoubleWord) pop2()
    else pop()

  sealed trait FallThrough extends Bytecode

  sealed abstract class Control extends Bytecode
  sealed abstract class Procedure extends Bytecode with FallThrough
  sealed abstract class Shuffle extends Bytecode with FallThrough {
    override type Self <: Shuffle
    override final def inputs = Seq.empty
    override final def output: Option[DataPort.Out] = None
  }

  sealed trait HasClassRef extends Bytecode {
    def classRef: ClassRef
    def withNewClassRef(newRef: ClassRef): Self
    final def rewriteClassRef(newRef: ClassRef): Self =
      if (classRef == newRef) self
      else withNewClassRef(newRef)
  }

  sealed trait HasMethodRef extends HasClassRef {
    override type Self <: HasMethodRef
    def methodRef: MethodRef
    def withNewMehtodRef(newRef: MethodRef): Self
    final def rewriteMethodRef(newRef: MethodRef): Self =
      if (methodRef == newRef) self
      else withNewMehtodRef(newRef)
    final def rewriteMethodRef(cr: ClassRef, mr: MethodRef): Self =
      rewriteClassRef(cr).rewriteMethodRef(mr).asInstanceOf[Self]
  }

  sealed trait HasFieldRef extends HasClassRef {
    override type Self <: HasFieldRef
    def fieldRef: FieldRef
    def withNewFieldRef(newRef: FieldRef): Self
    final def rewriteFieldRef(newRef: FieldRef): Self =
      if (fieldRef == newRef) self
      else withNewFieldRef(newRef)
    final def rewriteFieldRef(cr: ClassRef, fr: FieldRef): Self =
      rewriteClassRef(cr).rewriteFieldRef(fr).asInstanceOf[Self]
  }

  sealed trait HasJumpTargets extends Control {
    def jumpTargets: Set[JumpTarget]
  }

  sealed trait HasAJumpTarget extends HasJumpTargets {
    override def jumpTargets = Set(jumpTarget)
    def target: JumpTarget = jumpTarget // TODO: remove this
    def jumpTarget: JumpTarget
  }

  sealed abstract class Jump extends Control with HasAJumpTarget {
    override final def inputs = Seq.empty
    override final def output = None
    override final def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f)
  }
  sealed abstract class Branch extends Control with HasAJumpTarget with FallThrough
  sealed abstract class Exit extends Control
  sealed abstract class Return extends Exit
  sealed abstract class Throw extends Exit

  sealed abstract class XReturn extends Return {
    val retval: DataPort.In = DataPort.In("retval")
    override final val inputs = Seq(retval)
    override final def output = None
    override final def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f).ret(retval)
    def returnType: TypeRef.Public
  }
  // Void return
  sealed abstract class VoidReturn extends Return {
    override def inputs = Seq.empty
    override def output = None
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f)
  }

  sealed abstract class if_X1cmpXX extends Branch {
    val value1: DataPort.In = DataPort.In("value1")
    val value2: DataPort.In = DataPort.In("value2")
    override def inputs = Seq(value1, value2)
    override def output = None
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f).pop1(value2).pop1(value1)
  }

  sealed abstract class LocalAccess extends Shuffle {
    override type Self <: LocalAccess
    def localIndex: Int
    def rewriteLocalIndex(n: Int): Self
  }

  sealed abstract class Load1 extends LocalAccess {
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f).load1(localIndex)
  }
  sealed abstract class Load2 extends LocalAccess {
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f).load2(localIndex)
  }

  sealed abstract class Store1 extends LocalAccess {
    def storeType: TypeRef.SingleWord
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f).store1(storeType, localIndex)
  }

  sealed abstract class Store2 extends LocalAccess {
    def storeType: TypeRef.DoubleWord
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f).store2(storeType, localIndex)
  }

  sealed abstract class ConstX extends Procedure {
    def out: DataPort.Out
    def data: Data.Concrete
    override def inputs = Seq.empty
    override def output = Some(out)
  }

  sealed abstract class Const1 extends ConstX {
    final val out: DataPort.Out = DataPort.Out("const(1word)")
    override def nextFrame(l: Bytecode.Label, f: Frame) =
      update(l, f).push1(output, FrameItem(DataSource.Constant(l, out, data), data))
  }

  sealed abstract class Const2 extends ConstX {
    final val out: DataPort.Out = DataPort.Out("const(2word)")
    override def nextFrame(l: Bytecode.Label, f: Frame) =
      update(l, f).push2(output, FrameItem(DataSource.Constant(l, out, data), data))
  }

  sealed abstract class InvokeMethod extends Procedure with HasClassRef with HasMethodRef {
    override type Self <: InvokeMethod
    val args: Seq[DataPort.In] = methodRef.args.zipWithIndex.map { case (_, i) => DataPort.In(s"arg${i}") }
    val ret: Option[DataPort.Out] = if (methodRef.isVoid) None else Some(DataPort.Out("ret"))
    override final def output = ret
  }
  sealed abstract class InvokeClassMethod extends InvokeMethod {
    override type Self <: InvokeClassMethod
    override final def inputs = args
    override def nextFrame(l: Bytecode.Label, f: Frame) = {
      require(f.stack.size >= methodRef.args.size)
      val popped =
        args.zip(methodRef.args).foldRight(update(l, f)) {
          case ((a, t), u) =>
            if (t.isDoubleWord) u.pop2(a)
            else u.pop1(a)
        }
      ret.fold(popped) { rlabel =>
        popped.push(Some(rlabel), FrameItem(DataSource.MethodInvocation(l, rlabel), Data.Unknown(methodRef.ret)))
      }
    }
  }
  sealed abstract class InvokeInstanceMethod extends InvokeMethod {
    override type Self <: InvokeInstanceMethod
    val objectref: DataPort.In = DataPort.In("objectref")
    override final def inputs = objectref +: args
    override def nextFrame(l: Bytecode.Label, f: Frame) = {
      require(f.stack.size >= methodRef.args.size)
      val popped =
        args.zip(methodRef.args).foldRight(update(l, f)) {
          case ((a, t), u) =>
            if (t.isDoubleWord) u.pop2(a)
            else u.pop1(a)
        }.pop1(objectref)
      ret.fold(popped) { rlabel =>
        popped.push(Some(rlabel), FrameItem(DataSource.MethodInvocation(l, rlabel), Data.Unknown(methodRef.ret)))
      }
    }
    def resolveMethod(instance: Instance[_ <: AnyRef]): ClassRef = ???
  }

  sealed abstract class FieldAccess extends Procedure with HasClassRef with HasFieldRef {
    override type Self <: FieldAccess
  }
  sealed abstract class StaticFieldAccess extends FieldAccess {
    override type Self <: StaticFieldAccess
  }
  sealed abstract class InstanceFieldAccess extends FieldAccess {
    override type Self <: InstanceFieldAccess
    val objectref: DataPort.In = DataPort.In("objectref")
  }
  sealed trait FieldSetter extends FieldAccess {
    final val value = DataPort.In("value")
    final override def output = None
  }
  sealed trait FieldGetter extends FieldAccess {
    final val out = DataPort.Out("out")
    final override def output = Some(out)
  }

  case class nop() extends Shuffle {
    override type Self = nop
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f)
  }
  case class dup() extends Shuffle {
    override type Self = dup
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f).push(None, f.stack.head)
  }
  case class pop() extends Shuffle {
    override type Self = pop
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f).pop1()
  }
  case class pop2() extends Shuffle {
    override type Self = pop2
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f).pop2()
  }
  case class vreturn() extends VoidReturn {
    override type Self = vreturn
  }
  case class iload(override val localIndex: Int) extends Load1 {
    override type Self = iload
    override def rewriteLocalIndex(m: Int) = if (localIndex == m) self else iload(m)
  }
  case class aload(override val localIndex: Int) extends Load1 {
    override type Self = aload
    override def rewriteLocalIndex(m: Int) = if (localIndex == m) self else aload(m)
  }
  case class fload(override val localIndex: Int) extends Load1 {
    override type Self = fload
    override def rewriteLocalIndex(m: Int) = if (localIndex == m) self else fload(m)
  }
  case class dload(override val localIndex: Int) extends Load2 {
    override type Self = dload
    override def rewriteLocalIndex(m: Int) = if (localIndex == m) self else dload(m)
  }
  case class lload(override val localIndex: Int) extends Load2 {
    override type Self = lload
    override def rewriteLocalIndex(m: Int) = if (localIndex == m) self else lload(m)
  }
  case class istore(override val localIndex: Int) extends Store1 {
    override type Self = istore
    override def storeType = TypeRef.Int
    override def rewriteLocalIndex(m: Int) = if (localIndex == m) self else istore(m)
  }
  case class astore(override val localIndex: Int) extends Store1 {
    override type Self = astore
    override def storeType = TypeRef.Object
    override def rewriteLocalIndex(m: Int) = if (localIndex == m) self else astore(m)
  }
  case class dstore(override val localIndex: Int) extends Store2 {
    override type Self = dstore
    override def storeType = TypeRef.Double
    override def rewriteLocalIndex(m: Int) = if (localIndex == m) self else dstore(m)
  }

  case class ireturn() extends XReturn {
    override type Self = ireturn
    override def returnType = TypeRef.Int
  }
  case class freturn() extends XReturn {
    override type Self = freturn
    override def returnType = TypeRef.Float
  }
  case class dreturn() extends XReturn {
    override type Self = dreturn
    override def returnType = TypeRef.Double
  }
  case class lreturn() extends XReturn {
    override type Self = lreturn
    override def returnType = TypeRef.Long
  }
  case class areturn() extends XReturn {
    override type Self = areturn
    override def returnType = TypeRef.Reference(ClassRef.Object)
  }
  case class iconst(value: Int) extends Const1 {
    override type Self = iconst
    override def data = Data.ConcretePrimitive(TypeRef.Int, value)
  }
  case class lconst(value: Long) extends Const2 {
    override type Self = lconst
    override def data = Data.ConcretePrimitive(TypeRef.Long, value)
  }
  case class aconst_null() extends Const1 {
    override type Self = aconst_null
    override def data = Data.Null
  }
  case class ldc2_double(value: Double) extends Const2 {
    override type Self = ldc2_double
    override def data = Data.ConcretePrimitive(TypeRef.Double, value)
  }
  case class goto(override val jumpTarget: JumpTarget = JumpTarget("jump")) extends Jump {
    override type Self = goto
  }
  case class if_icmple(override val jumpTarget: JumpTarget) extends if_X1cmpXX {
    override type Self = if_icmple
  }
  case class if_icmpge(override val jumpTarget: JumpTarget) extends if_X1cmpXX {
    override type Self = if_icmpge
  }
  case class if_acmpne(override val jumpTarget: JumpTarget) extends if_X1cmpXX {
    override type Self = if_acmpne
  }
  case class ifnonnull(override val jumpTarget: JumpTarget) extends Branch {
    override type Self = ifnonnull
    val value: DataPort.In = DataPort.In("value")
    override def pretty = "ifnonnull"
    override def inputs = Seq(value)
    override def output = None
    override def nextFrame(l: Bytecode.Label, f: Frame) = update(l, f).pop1(value)
  }
  sealed abstract class PrimitiveBinOp[A <: AnyVal] extends Procedure {
    val value1 = DataPort.In("value1")
    val value2 = DataPort.In("value2")
    val result = DataPort.Out("result")

    def operandType: TypeRef.Primitive
    def op(value1: A, value2: A): A

    override def inputs = Seq(value1, value2)
    override def output = Some(result)
    override def nextFrame(l: Bytecode.Label, f: Frame) =
      (f.stack(0), f.stack(operandType.wordSize)) match {
        case (d1, d2) if d1.data.typeRef == operandType && d2.data.typeRef == operandType =>
          update(l, f)
            .pop(operandType, value2)
            .pop(operandType, value1)
            .push(
              Some(result),
              FrameItem(
                DataSource.Generic(l, result),
                d1.data.value.flatMap { v1 =>
                  d2.data.value.map { v2 =>
                    Data.ConcretePrimitive(
                      operandType,
                      op(v1.asInstanceOf[A], v2.asInstanceOf[A])
                    )
                  }
                }.getOrElse { Data.Unknown(operandType) }
              )
            )
        case (d1, d2) => throw new AnalyzeException(s"$this: Type error: ${(d1, d2)}")
      }
  }
  sealed abstract class PrimitiveUniOp[A <: AnyVal, B <: AnyVal] extends Procedure {
    val value = DataPort.In("value")
    val result = DataPort.Out("result")

    override def inputs = Seq(value)
    override def output = Some(result)

    def operandType: TypeRef.Primitive
    def resultType: TypeRef.Primitive
    def op(value: A): B

    override def nextFrame(l: Bytecode.Label, f: Frame) = {
      update(l, f)
        .pop(operandType)
        .push(
          Some(result),
          FrameItem(
            DataSource.Generic(l, result),
            f.stackTop.data.value.map { v =>
              Data.ConcretePrimitive(resultType, op(v.asInstanceOf[A]))
            }.getOrElse { Data.Unknown(resultType) }
          )
        )
    }
  }
  case class iadd() extends PrimitiveBinOp[Int] {
    override type Self = iadd
    override def operandType = TypeRef.Int
    override def op(value1: Int, value2: Int) = value1 + value2
  }
  case class dadd() extends PrimitiveBinOp[Double] {
    override type Self = dadd
    override def operandType = TypeRef.Double
    override def op(value1: Double, value2: Double) = value1 + value2
  }
  case class isub() extends PrimitiveBinOp[Int] {
    override type Self = isub
    override def operandType = TypeRef.Int
    override def op(value1: Int, value2: Int) = value1 - value2
  }
  case class dsub() extends PrimitiveBinOp[Double] {
    override type Self = dsub
    override def operandType = TypeRef.Double
    override def op(value1: Double, value2: Double) = value1 - value2
  }
  case class dmul() extends PrimitiveBinOp[Double] {
    override type Self = dmul
    override def operandType = TypeRef.Double
    override def op(value1: Double, value2: Double) = value1 * value2
  }
  case class imul() extends PrimitiveBinOp[Int] {
    override type Self = imul
    override def operandType = TypeRef.Int
    override def op(value1: Int, value2: Int) = value1 * value2
  }
  case class d2i() extends PrimitiveUniOp[Double, Int] {
    override type Self = d2i
    override def operandType = TypeRef.Double
    override def resultType = TypeRef.Int
    override def op(value: Double) = value.toInt
  }
  case class i2d() extends PrimitiveUniOp[Int, Double] {
    override type Self = i2d
    override def operandType = TypeRef.Int
    override def resultType = TypeRef.Double
    override def op(value: Int) = value.toDouble
  }
  case class invokevirtual(override val classRef: ClassRef, override val methodRef: MethodRef) extends InvokeInstanceMethod {
    override type Self = invokevirtual
    override def withNewClassRef(newRef: ClassRef) = copy(classRef = newRef)
    override def withNewMehtodRef(newRef: MethodRef) = copy(methodRef = newRef)
    override def pretty = s"invokevirtual ${classRef.pretty}.${methodRef.str}"
  }
  case class invokeinterface(override val classRef: ClassRef, override val methodRef: MethodRef, count: Int) extends InvokeInstanceMethod {
    override type Self = invokeinterface
    override def withNewClassRef(newRef: ClassRef) = copy(classRef = newRef)
    override def withNewMehtodRef(newRef: MethodRef) = copy(methodRef = newRef)
    override def pretty = s"invokeinterface ${classRef.pretty}.${methodRef.str}"
  }
  case class invokespecial(override val classRef: ClassRef, override val methodRef: MethodRef) extends InvokeInstanceMethod {
    override type Self = invokespecial
    override def withNewClassRef(newRef: ClassRef) = copy(classRef = newRef)
    override def withNewMehtodRef(newRef: MethodRef) = copy(methodRef = newRef)
    override def pretty = s"invokespecial ${classRef.pretty}.${methodRef.str}"
    override def nextFrame(l: Bytecode.Label, f: Frame) = {
      require(f.stack.size >= methodRef.args.size + 1)
      f.stack(methodRef.args.size) match {
        case FrameItem(ds @ DataSource.New(sl, sp), Data.Uninitialized(t)) =>
          // should ctor
          if(!methodRef.isInit) {
            throw new IllegalArgumentException(s"expect ctor call but ${methodRef}")
          }
          if(t != classRef) {
            throw new IllegalArgumentException(s"Illegal ctor invocation: instance is $t and ctor is $classRef.$methodRef")
          }
          if(!classRef.isInstanceOf[ClassRef.Concrete]) {
            throw new IllegalArgumentException(s"Unexpected abstract ClassRef: ${classRef}")
          }
          assert(methodRef.ret == TypeRef.Void)
          val popped =
            args.zip(methodRef.args).foldRight(update(l, f)) {
              case ((a, t), u) =>
                if (t.isDoubleWord) u.pop2(a)
                else u.pop1(a)
            }.pop1(objectref)
          popped.copy(
            frameItems =
              popped.frameItems + ((sl, sp) -> FrameItem(
                ds,
                Data.AbstractReference(
                  new Instance.New[AnyRef](
                    classRef.asInstanceOf[ClassRef.Concrete].loadKlass,
                    methodRef.descriptor
                  )
                )
              ))
          )
        case FrameItem(src, Data.Uninitialized(t)) if src.must(DataSource.This) =>
          // super ctor call in ctor
          super.nextFrame(l, f)
        case FrameItem(src, Data.Uninitialized(t)) =>
          // unexpected
          throw new IllegalArgumentException(s"Unexpected ctor call: data src = $src")
        case fi => // normal
          if(methodRef.isInit) {
            throw new IllegalArgumentException(s"Unexpected ctor call: data src = ${fi.source}")
          }
          super.nextFrame(l, f)
      }
    }
  }
  case class invokestatic(override val classRef: ClassRef, override val methodRef: MethodRef) extends InvokeClassMethod {
    override type Self = invokestatic
    override def withNewClassRef(newRef: ClassRef) = copy(classRef = newRef)
    override def withNewMehtodRef(newRef: MethodRef) = copy(methodRef = newRef)
    override def pretty = s"invokestatic ${classRef.pretty}.${methodRef.str}"
  }
  case class getfield(override val classRef: ClassRef, override val fieldRef: FieldRef) extends InstanceFieldAccess with FieldGetter {
    override type Self = getfield
    override def withNewClassRef(newRef: ClassRef) = copy(classRef = newRef)
    override def withNewFieldRef(newRef: FieldRef) = copy(fieldRef = newRef)

    override def inputs = Seq(objectref)

    override def pretty = s"getfield ${classRef}.${fieldRef}"
    override def nextFrame(l: Bytecode.Label, f: Frame) = {
      val self = f.stack(0).data
      val data =
        self match {
          case d: Data.Reference =>
            val key = d.instance.resolveField(classRef, fieldRef) -> fieldRef
            val field = d.instance.klass.instanceFieldAttributes(key)
            if (field.isFinal) d.instance.fieldValues(key)
            else Data.Unknown(fieldRef.descriptor.typeRef)
          case _ =>
            Data.Unknown(fieldRef.descriptor.typeRef)
        }
      update(l, f).pop1(objectref).push(output, FrameItem(DataSource.InstanceField(l, out, self, classRef, fieldRef), data))
    }
  }
  case class getstatic(override val classRef: ClassRef, override val fieldRef: FieldRef) extends StaticFieldAccess with FieldGetter {
    override type Self = getstatic
    override def withNewClassRef(newRef: ClassRef) = copy(classRef = newRef)
    override def withNewFieldRef(newRef: FieldRef) = copy(fieldRef = newRef)

    override def inputs = Seq()

    override def pretty = s"getstatic ${fieldRef}"
    override def nextFrame(l: Bytecode.Label, f: Frame) = {
      val data = Data.Unknown(fieldRef.descriptor.typeRef) // TODO: set static field value if it is final
      update(l, f).push(output, FrameItem(DataSource.StaticField(l, out, classRef, fieldRef), data))
    }
  }
  case class putfield(override val classRef: ClassRef, override val fieldRef: FieldRef) extends InstanceFieldAccess with FieldSetter {
    override type Self = putfield
    override def withNewClassRef(newRef: ClassRef) = copy(classRef = newRef)
    override def withNewFieldRef(newRef: FieldRef) = copy(fieldRef = newRef)
    override def inputs = Seq(objectref)
    override def pretty = s"putfield ${classRef}.${fieldRef}"
    override def nextFrame(l: Bytecode.Label, f: Frame) =
      update(l, f).pop(fieldRef.descriptor.typeRef, value).pop1(objectref)
  }
  case class athrow() extends Throw {
    override type Self = athrow
    val objectref = DataPort.In("objectref")
    override def pretty = s"athrow"
    override def inputs = Seq(objectref)
    override def output = None
    override def nextFrame(l: Bytecode.Label, f: Frame) =
      update(l, f).athrow(objectref)
  }
  case class new_(override val classRef: ClassRef) extends Procedure with HasClassRef {
    val objectref = DataPort.Out("new")
    override type Self = new_
    override def withNewClassRef(cr: ClassRef) = copy(classRef = cr)
    override def inputs = Seq()
    override def output = Some(objectref)
    override def nextFrame(l: Bytecode.Label, f: Frame) =
      update(l, f).push(output, FrameItem(DataSource.New(l, objectref), Data.Uninitialized(classRef)))
  }
}
