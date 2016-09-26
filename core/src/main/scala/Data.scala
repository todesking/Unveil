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
    else Data.Unsure(TypeRef.SecondWord)

  override def toString: String = s"""${typeRef} = ${valueString}"""
}
object Data {
  def merge(d1: Data, d2: Data): Data = {
    if (d1 eq d2) d1
    else (d1, d2) match {
      case (Unsure(t), rhs) =>
        Unsure(TypeRef.common(t, rhs.typeRef))
      case (lhs, Unsure(t)) =>
        Unsure(TypeRef.common(t, lhs.typeRef))
      case (Primitive(t1, v1), Primitive(t2, v2)) if t1 == t2 && v1 == v2 =>
        Primitive(t1, v1)
      case (Reference(t1, i1), Reference(t2, i2)) if i1 == i2 =>
        Reference(TypeRef.common(t1, t2).asInstanceOf[TypeRef.Reference], i1)
      case (Null, Null) => Null
      case (d1, d2) =>
        Unsure(TypeRef.common(d1.typeRef, d2.typeRef))
    }
  }

  sealed abstract class Known extends Data {
  }

  sealed abstract class Concrete extends Known {
    def concreteValue: Any
    override final def value = Some(concreteValue)
  }

  case class Unsure(override val typeRef: TypeRef) extends Data {
    override def valueString = "???"
    override def value = None
  }

  case class Uninitialized(override val typeRef: TypeRef.Reference) extends Known {
    override val valueString = s"new $typeRef(...)"
    override val value = None
    override def equals(rhs: Any): Boolean = rhs match {
      case rhs: AnyRef => rhs eq this
      case _ => false
    }
  }

  case class Primitive(override val typeRef: TypeRef.Primitive, override val concreteValue: AnyVal) extends Concrete {
    override def valueString = concreteValue.toString
  }
  case class Reference(override val typeRef: TypeRef.Reference, instance: Instance[_ <: AnyRef]) extends Concrete {
    def classRef: ClassRef = typeRef.classRef
    override def valueString = "<object>"
    override def concreteValue = instance.materialized.value
    override def isInstance(i: Instance[_ <: AnyRef]) = instance == i
  }
  case object Null extends Concrete {
    override def valueString = "null"
    override val typeRef = TypeRef.Null
    override val concreteValue = null
  }
}

