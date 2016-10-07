package com.todesking.unveil

sealed abstract class DataSource {
  def merge(rhs: DataSource): DataSource
  def is(rhs: DataSource.Single): Option[Boolean]
  def may(rhs: DataSource.Single): Boolean =
    is(rhs) match {
      case Some(true) | None => true
      case Some(false) => false
    }
  def must(rhs: DataSource.Single): Boolean =
    is(rhs) match {
      case Some(true) => true
      case Some(false) => false
      case None => throw new RuntimeException("ambigious")
    }
  def mayProducedBy(l: Bytecode.Label, p: DataPort.Out): Boolean =
    producedBy(l, p) != Some(false)
  def producedBy(l: Bytecode.Label, p: DataPort.Out): Option[Boolean]
  def unambiguous: Boolean
  def mayFieldAccess(cr: ClassRef, fr: FieldRef): Boolean
  def single: Option[DataSource.Single]
}
object DataSource {
  case class Multiple(sources: Set[DataSource.Single]) extends DataSource {
    require(sources.nonEmpty)
    override def merge(rhs: DataSource): DataSource = rhs match {
      case Multiple(ss) => Multiple(sources ++ ss)
      case s: Single => Multiple(sources + s)
    }
    override def is(rhs: DataSource.Single): Option[Boolean] =
      if (sources.contains(rhs)) {
        if (sources.size == 1) Some(true)
        else None
      } else {
        Some(false)
      }
    override def unambiguous = sources.size == 1
    override def mayFieldAccess(cr: ClassRef, fr: FieldRef) =
      sources.exists(_.mayFieldAccess(cr, fr))
    override def single = if(sources.size == 1) Some(sources.head) else None
    override def producedBy(l: Bytecode.Label, p: DataPort.Out): Option[Boolean] = {
      val parts = sources.map(_.producedBy(l, p))
      if(parts.forall(_ == Some(true)))  Some(true)
      else if(parts.forall(_ == Some(false))) Some(false)
      else None
    }
  }

  sealed abstract class Single extends DataSource {
    override def merge(rhs: DataSource): DataSource = rhs match {
      case Multiple(sources) => Multiple(sources + this)
      case s: Single => Multiple(Set(this, s))
    }
    override def is(rhs: DataSource.Single): Option[Boolean] =
      Some(this == rhs)
    override def unambiguous = true
    override def mayFieldAccess(cr: ClassRef, fr: FieldRef) = false
    override def single = Some(this)
    override def producedBy(l: Bytecode.Label, p: DataPort.Out): Option[Boolean] = Some(false)
  }

  sealed trait HasLocation extends Single {
    def label: Bytecode.Label
    def port: DataPort.Out
    final override def producedBy(l: Bytecode.Label, p: DataPort.Out): Option[Boolean] = Some(l == label && p == port)
  }
  object HasLocation {
    def unapply(s: DataSource): Option[(Bytecode.Label, DataPort.Out)] = s match {
      case s: HasLocation => Some((s.label, s.port))
      case s => None
    }
  }

  case object This extends Single
  case class Argument(n: Int) extends Single
  case object Constant extends Single

  case class InstanceField(
    override val label: Bytecode.Label,
    override val port: DataPort.Out,
    target: Data,
    classRef: ClassRef,
    fieldRef: FieldRef
  ) extends HasLocation {
    override def mayFieldAccess(cr: ClassRef, fr: FieldRef) =
      cr == classRef && fr == fieldRef
  }
  case class StaticField(
    override val label: Bytecode.Label,
    override val port: DataPort.Out,
    classRef: ClassRef,
    fieldRef: FieldRef
  ) extends HasLocation {
    override def mayFieldAccess(cr: ClassRef, fr: FieldRef) =
      cr == classRef && fr == fieldRef
  }
  case class New(
    override val label: Bytecode.Label,
    override val port: DataPort.Out
  ) extends HasLocation
  case class MethodInvocation(
    override val label: Bytecode.Label,
    override val port: DataPort.Out
  ) extends HasLocation
  case class Constant(
    override val label: Bytecode.Label,
    override val port: DataPort.Out,
    data: Data.Concrete
  ) extends HasLocation
  case class Generic(
    override val label: Bytecode.Label,
    override val port: DataPort.Out
  ) extends HasLocation
}
