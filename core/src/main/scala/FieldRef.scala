package com.todesking.unveil

case class FieldRef(name: String, descriptor: FieldDescriptor) {
  override def toString: String = s"${name}: ${descriptor}"
  def typeRef: TypeRef = descriptor.typeRef
  def renamed(newName: String): FieldRef = copy(name = newName)
  def anotherUniqueName(baseNames: String*): FieldRef =
    if(baseNames.isEmpty) anotherUniqueName(name)
    else copy(name = FieldRef.uniqueName(baseNames: _*))
}
object FieldRef {
  def from(f: java.lang.reflect.Field): FieldRef =
    FieldRef(f.getName, FieldDescriptor.from(f))

  val uniqueName = new UniqueNamer
}
