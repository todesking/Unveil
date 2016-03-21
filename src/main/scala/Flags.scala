package com.todesking.unveil

trait Flags[Type <: Flags[Type]] {
  def |(that: Flags[Type]): Type
  def enabledIn(flags: Int): Boolean
  def has(flags: Flags[Type]): Boolean
  def toInt: Int
}

trait FlagsCompanion[Type <: Flags[Type]] {
  def multi(items: Set[SingleFlag]): Type

  trait MultiFlags extends Flags[Type] {
    def items: Set[SingleFlag]

    override def |(that: Flags[Type]): Type = that match {
      case that: MultiFlags => multi(items ++ that.items)
      case that: SingleFlag => multi(items + that)
    }

    override def enabledIn(flags: Int) = items.forall(_.enabledIn(flags))

    override def has(flags: Flags[Type]) = flags match {
      case that: MultiFlags => that.items.subsetOf(this.items)
      case that: SingleFlag => items.contains(that)
    }

    override def toString = s"${items.mkString(", ")}"

    override def toInt = items.foldLeft[Int](0)(_ | _.toInt)
  }
  trait SingleFlag extends Flags[Type] {
    override def |(that: Flags[Type]): Type = that match {
      case that: MultiFlags => multi(that.items + this)
      case that: SingleFlag => multi(Set(this, that))
    }

    override def enabledIn(flags: Int) =
      (flags & toInt) == toInt

    override def has(flags: Flags[Type]): Boolean = flags match {
      case that: MultiFlags => that.items.forall(has(_))
      case that: SingleFlag => this == that
    }
  }
}
