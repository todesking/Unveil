package com.todesking.unveil

import java.lang.reflect.{ Method => JMethod, Constructor }

case class MethodDescriptor(ret: TypeRef.Public, args: Seq[TypeRef.Public]) {
  def str: String = s"${args.map(_.str).mkString("(", "", ")")}${ret.str}"

  override def toString = s"${args.mkString("(", ", ", ")")}${ret}"

  def isVoid: Boolean = ret == TypeRef.Void

  def argToLocalIndex(arg: Int, isStatic: Boolean): Int =
    (if (isStatic) 0 else 1) + argToLocalTable(arg)

  private[this] lazy val argToLocalTable: Seq[Int] = {
    var n = 0
    val t = scala.collection.mutable.ArrayBuffer.empty[Int]
    args.zipWithIndex foreach {
      case (arg, i) =>
        t(i) = n
        n += arg.wordSize
    }
    t.toSeq
  }

}
object MethodDescriptor {
  def parse(src: String, cl: ClassLoader): MethodDescriptor =
    Parsers.parseMethodDescriptor(src, cl)

  def from(m: JMethod): MethodDescriptor =
    MethodDescriptor(TypeRef.from(m.getReturnType), m.getParameterTypes.map(TypeRef.from).toSeq)

  def from(m: Constructor[_]): MethodDescriptor =
    MethodDescriptor(TypeRef.Void, m.getParameterTypes.map(TypeRef.from).toSeq)
}
