package com.todesking.unveil

class UnveilException(msg: String, err: Throwable) extends RuntimeException(msg, err) {
  def this(msg: String) = this(msg, null)
}

object UnveilException {
  trait HasMethodBody extends UnveilException {
    def methodBody: MethodBody
  }
  trait HasMethodLocation extends UnveilException {
    def methodLocation: (ClassRef, MethodRef)
  }
  trait HasFieldLocation extends UnveilException {
    def fieldLocation: (ClassRef, MethodRef)
  }
}

class MaterializeException(msg: String, err: Throwable) extends UnveilException(msg, err)

class InvalidClassException(val klass: Klass, err: LinkageError)
  extends MaterializeException(err.toString, err)

class AnalyzeException(msg: String) extends UnveilException(msg)

class MethodAnalyzeException(classRef: ClassRef, methodRef: MethodRef, msg: String)
  extends AnalyzeException(s"Method analyze failed(${classRef}.${methodRef}): ${msg}")

class MethodBodyAnalyzeException(override val methodBody: MethodBody, msg: String)
  extends AnalyzeException(s"Method analyze failed(descriptor: ${methodBody.descriptor}): ${msg}") with UnveilException.HasMethodBody

class UnsupportedOpcodeException(classRef: ClassRef, methodRef: MethodRef, byte: Int)
  extends MethodAnalyzeException(classRef, methodRef, f"Unsupported opcode: 0x$byte%02X")

class FieldAnalyzeException(classRef: ClassRef, fieldRef: FieldRef, msg: String)
  extends AnalyzeException(s"Field analyze failed(${classRef}.${fieldRef}): ${msg}")

class TransformException(msg: String) extends UnveilException(msg)

class FieldTransformException(classRef: ClassRef, fieldRef: FieldRef, msg: String)
  extends AnalyzeException(s"Transform failed at ${classRef}.${fieldRef}: ${msg}")

class MethodTransformException(classRef: ClassRef, methodRef: MethodRef, msg: String)
  extends AnalyzeException(s"Transform failed at ${classRef}.${methodRef}: ${msg}")

class BytecodeTransformException(val classRef: ClassRef, val methodRef: MethodRef, override val methodBody: MethodBody, val bytecode: Bytecode, msg: String)
  extends MethodTransformException(classRef, methodRef, s"$bytecode: $msg") with UnveilException.HasMethodBody

class UnveilBugException(msg: String, err: Throwable) extends RuntimeException(msg, err) {
  def detail: String = ""
}
