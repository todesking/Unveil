package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

import java.lang.reflect.{ Method => JMethod, Field => JField, Modifier }

// TODO: remove whole
case class Field(
    descriptor: FieldDescriptor,
    attribute: FieldAttribute,
    data: Data.Concrete
) {
  def isFinal: Boolean = attribute.isFinal
}
object Field {
  def from(f: JField, obj: AnyRef): Field =
    Field(
      FieldDescriptor.from(f),
      FieldAttribute.from(f),
      data(f, obj)
    )

  private[this] def data(f: JField, obj: AnyRef): Data.Concrete = {
    val v = f.get(obj)
    TypeRef.from(f.getType) match {
      case t: TypeRef.Primitive => Data.ConcretePrimitive(t, v.asInstanceOf[AnyVal])
      case t: TypeRef.Reference if v == null => Data.Null
      case t: TypeRef.Reference => Data.ConcreteReference(Instance.of(v))
    }
  }
}
