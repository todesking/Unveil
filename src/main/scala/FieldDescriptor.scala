package com.todesking.unveil

case class FieldDescriptor(typeRef: TypeRef.Public) {
  def str = typeRef.str
  override def toString = typeRef.toString
}
object FieldDescriptor {
  def from(f: java.lang.reflect.Field): FieldDescriptor =
    FieldDescriptor(TypeRef.from(f.getType))
  def parse(src: String, cl: ClassLoader): FieldDescriptor =
    Parsers.parseFieldDescriptor(src, cl)
}
