package com.todesking.unveil

object Equality {
  trait Reference extends AnyRef {
    final override def equals(rhs: Any): Boolean = rhs match {
      case rhs: AnyRef => this eq rhs
      case _ => false
    }
    final override def hashCode: Int =
      java.lang.System.identityHashCode(this)
  }
  trait Delegate extends AnyRef with scala.Equals {
    def equalityObject: Any

    final override def equals(rhs: Any): Boolean = rhs match {
      case rhs: scala.Equals with Delegate =>
        rhs.canEqual(this) && equalityObject == rhs.equalityObject
      case _ => false
    }

    final override def hashCode: Int = equalityObject.hashCode
  }
}
