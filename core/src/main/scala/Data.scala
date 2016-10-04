package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

sealed abstract class Data {
  def typeRef: TypeRef
  def value: Option[Any]
  def valueString: String
  def isInstance(instance: Instance[_ <: AnyRef]): Boolean = false

  def merge(rhs: Data): Data = Data.merge(this, rhs)

  def secondWordData: Data =
    if (!typeRef.isDoubleWord) throw new IllegalArgumentException()
    else Data.Unknown(TypeRef.SecondWord)

  override def toString: String = s"""${typeRef} = ${valueString}"""
}
object Data {
  def merge(d1: Data, d2: Data): Data = {
    if (d1 eq d2) d1
    else (d1, d2) match {
      case (Unknown(t), rhs) =>
        Unknown(TypeRef.common(t, rhs.typeRef))
      case (lhs, Unknown(t)) =>
        Unknown(TypeRef.common(t, lhs.typeRef))
      case (ConcretePrimitive(t1, v1), ConcretePrimitive(t2, v2)) if t1 == t2 && v1 == v2 =>
        ConcretePrimitive(t1, v1)
      case (ConcreteReference(i1), ConcreteReference(i2)) if i1 == i2 =>
        ConcreteReference(i1)
      case (Null, Null) => Null
      // TODO: NewReference
      case (d1, d2) =>
        Unknown(TypeRef.common(d1.typeRef, d2.typeRef))
    }
  }

  sealed abstract class Known extends Data {
  }

  case class Unknown(override val typeRef: TypeRef) extends Data with Equality.Reference {
    override def valueString = "???"
    override def value = None
  }

  sealed abstract class Concrete extends Known {
    def concreteValue: Any
    override final def value = Some(concreteValue)
  }

  case class Uninitialized(override val typeRef: TypeRef.Reference) extends Known with Equality.Reference {
    override val valueString = s"new $typeRef(uninitialized)"
    override val value = None
  }
  case class ConcretePrimitive(override val typeRef: TypeRef.Primitive, override val concreteValue: AnyVal) extends Concrete {
    // TODO: require(typeRef.isValue(concreteValue))
    override def valueString = concreteValue.toString
  }
  case class ConcreteReference(instance: Instance.Concrete[_ <: AnyRef]) extends Concrete {
    override def typeRef: TypeRef.Reference = instance.thisRef.toTypeRef
    def classRef: ClassRef = typeRef.classRef
    override def valueString = "<object>"
    override def concreteValue = instance.materialized.value
    override def isInstance(i: Instance[_ <: AnyRef]) = instance == i
  }
  case class NewReference(instance: Instance.New[_ <: AnyRef]) extends Known {
    override def typeRef = instance.thisRef.toTypeRef
    override def value = None
    override def valueString = instance.toString
  }
  case object Null extends Concrete {
    override def valueString = "null"
    override val typeRef = TypeRef.Null
    override val concreteValue = null
  }
}

