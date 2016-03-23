package com.todesking.unveil

import java.lang.reflect.{ Method => JMethod, Modifier }

sealed abstract class MethodAttribute extends Flags[MethodAttribute] {
  def isVirtual: Boolean =
    !this.has(MethodAttribute.Private) && !this.has(MethodAttribute.Static)
  def isFinal: Boolean =
    this.has(MethodAttribute.Final)
  def isStatic: Boolean =
    this.has(MethodAttribute.Static)
  def isAbstract: Boolean =
    this.has(MethodAttribute.Abstract)
  def isNative: Boolean =
    this.has(MethodAttribute.Native)
}
object MethodAttribute extends FlagsCompanion[MethodAttribute] {
  def from(m: JMethod): MethodAttribute =
    from(m.getModifiers)

  def from(flags: Int): MethodAttribute =
    items.filter(_.enabledIn(flags)).foldLeft[MethodAttribute](empty)(_ | _)

  val empty = Multi(Set.empty)

  override def multi(items: Set[SingleFlag]): MethodAttribute =
    Multi(items)

  case class Multi(override val items: Set[SingleFlag]) extends MethodAttribute with MultiFlags

  sealed abstract class Single(override val toInt: Int) extends MethodAttribute with SingleFlag

  case object Public extends Single(Modifier.PUBLIC)
  case object Private extends Single(Modifier.PRIVATE)
  case object Protected extends Single(Modifier.PROTECTED)
  case object Native extends Single(Modifier.NATIVE)
  case object Abstract extends Single(Modifier.ABSTRACT)
  case object Final extends Single(Modifier.FINAL)
  case object Synchronized extends Single(Modifier.SYNCHRONIZED)
  case object Strict extends Single(Modifier.STRICT)
  case object Static extends Single(Modifier.STATIC)

  val items = Seq(Public, Private, Protected, Native, Abstract, Final, Synchronized, Strict, Static)
}

