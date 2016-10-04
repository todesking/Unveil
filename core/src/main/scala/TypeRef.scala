package com.todesking.unveil

sealed abstract class TypeRef {
  def isDoubleWord: Boolean
  def wordSize: Int = if (isDoubleWord) 2 else 1
  def isAssignableFrom(rhs: TypeRef): Boolean =
    (this == rhs) || ((this, rhs) match {
      case (l: TypeRef.Reference, r: TypeRef.Reference) if l.classRef >= r.classRef => true
      case _ => false
    })
}
object TypeRef {
  def parse(src: String, cl: ClassLoader): TypeRef.Public =
    Parsers.parseTypeRef(src, cl)

  def from(c: Class[_]): Public = {
    if (c == java.lang.Integer.TYPE) Int
    else if (c == Long.javaClass) Long
    else if (c == Char.javaClass) Char
    else if (c == Byte.javaClass) Byte
    else if (c == Boolean.javaClass) Boolean
    else if (c == Short.javaClass) Short
    else if (c == Float.javaClass) Float
    else if (c == Double.javaClass) Double
    else if (c == Void.javaClass) Void
    else if (c.isArray) throw new UnveilException("NIMPL") // ???
    else Reference(ClassRef.of(c))
  }

  def common(t1: TypeRef, t2: TypeRef): TypeRef =
    (t1, t2) match {
      case (t1, t2) if t1 == t2 => t1
      case (Undefined, _) => Undefined
      case (_, Undefined) => Undefined
      case (Null, ref @ Reference(_)) => ref
      case (ref @ Reference(_), Null) => ref
      case (r1 @ Reference(_), r2 @ Reference(_)) =>
        ???
      case (_: Primitive, _: Primitive) => Undefined
      case (SecondWord, _) => Undefined
      case (_, SecondWord) => Undefined
    }

  sealed trait SingleWord extends TypeRef {
    override def isDoubleWord = false
  }
  sealed trait DoubleWord extends TypeRef {
    override def isDoubleWord = true
  }

  case object Undefined extends TypeRef with SingleWord {
    override def toString = "[undefined]"
  }
  case object SecondWord extends TypeRef with SingleWord {
    override def toString = "[second word]"
  }
  case object Null extends TypeRef with SingleWord {
    override def toString = "[null]"
  }

  sealed abstract class Public extends TypeRef {
    def str: String
    def javaClass: Class[_]
    def defaultValue: Any
  }

  sealed abstract class Primitive(
    override val toString: String,
    override val str: String,
    override val javaClass: Class[_],
    override val defaultValue: Any
  ) extends Public

  case object Byte extends Primitive("int", "B", java.lang.Byte.TYPE, 0.toByte) with SingleWord
  case object Boolean extends Primitive("bool", "Z", java.lang.Boolean.TYPE, false) with SingleWord
  case object Char extends Primitive("char", "C", java.lang.Character.TYPE, '\u0000') with SingleWord
  case object Short extends Primitive("short", "S", java.lang.Short.TYPE, 0.toShort) with SingleWord
  case object Int extends Primitive("int", "I", java.lang.Integer.TYPE, 0) with SingleWord
  case object Float extends Primitive("float", "F", java.lang.Float.TYPE, 0.0f) with SingleWord
  case object Long extends Primitive("long", "J", java.lang.Long.TYPE, 0L) with DoubleWord
  case object Double extends Primitive("double", "D", java.lang.Double.TYPE, 0.0) with DoubleWord
  case object Void extends Primitive("void", "V", java.lang.Void.TYPE, null) with SingleWord

  case class Reference(classRef: ClassRef) extends Public with SingleWord {
    override def str = s"L${classRef.binaryName};"
    override def toString = classRef.toString
    override def defaultValue = null
    // TODO: It smells..
    override def javaClass = classRef match {
      case c: ClassRef.Concrete => c.loadClass
      case c: ClassRef.Extend => throw new IllegalStateException()
    }
  }

  val Object: Reference = Reference(ClassRef.Object)
}
