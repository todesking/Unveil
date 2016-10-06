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
      case (AbstractReference(i1), AbstractReference(i2)) if i1 == i2 =>
        AbstractReference(i1)
      case (Null, Null) => Null
      case (d1, d2) =>
        Unknown(TypeRef.common(d1.typeRef, d2.typeRef))
    }
  }

  def reference(i: Instance[_ <: AnyRef]): Reference = i match {
    case i: Instance.Concrete[_] => ConcreteReference(i)
    case i: Instance.Abstract[_] => AbstractReference(i)
  }

  sealed abstract class Known extends Data {
  }

  case class Unknown(override val typeRef: TypeRef) extends Data with Equality.Reference {
    override def valueString = "???"
    override def value = None
  }

  case class UnknownReference(
      klass: Klass,
      fieldValues: Map[(ClassRef, FieldRef), Data]
  ) extends Data with Equality.Reference {
    override def typeRef = klass.ref.toTypeRef
    override def valueString = "???"
    override def value = None
  }

  sealed abstract class Concrete extends Known {
    def concreteValue: Any
  }

  case class Uninitialized(classRef: ClassRef) extends Known with Equality.Reference {
    override def typeRef = classRef.toTypeRef
    override val valueString = s"new $typeRef(uninitialized)"
    override val value = None
  }
  case class ConcretePrimitive(override val typeRef: TypeRef.Primitive, override val concreteValue: AnyVal) extends Concrete {
    // TODO: require(typeRef.isValue(concreteValue))
    override def valueString = concreteValue.toString
    override def value = Some(concreteValue)
  }

  case object Null extends Concrete {
    override def valueString = "null"
    override val typeRef = TypeRef.Null
    override val concreteValue = null
    override def value = Some(concreteValue)
  }

  sealed trait Reference extends Known with Equality.Delegate {
    override def canEqual(rhs: Any) = rhs.isInstanceOf[Reference]
    override def equalityObject = instance

    val instance: Instance[_ <: AnyRef]
    override def typeRef = instance.thisRef.toTypeRef
    override def value = instance.valueOption
    override def valueString = instance.toString
  }

  case class ConcreteReference(override val instance: Instance.Concrete[_ <: AnyRef]) extends Concrete with Reference {
    override def concreteValue = instance.value
  }

  case class AbstractReference(override val instance: Instance.Abstract[_ <: AnyRef]) extends Known with Reference {
  }
}

