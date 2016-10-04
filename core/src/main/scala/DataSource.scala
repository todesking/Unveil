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
}
object DataSource {
  case class Multiple(sources: Set[DataSource.Single]) extends DataSource {
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
  }

  sealed abstract class Single extends DataSource {
    override def merge(rhs: DataSource): DataSource = rhs match {
      case Multiple(sources) => Multiple(sources + this)
      case s: Single => Multiple(Set(this, s))
    }
    override def is(rhs: DataSource.Single): Option[Boolean] =
      Some(this == rhs)
  }

  case object This extends Single
  case class Argument(n: Int) extends Single
  case class Field(classRef: ClassRef, fieldRef: FieldRef) extends Single
  import com.todesking.unveil.{ Bytecode => TopBytecode }
  case class Bytecode(label: TopBytecode.Label, port: DataPort.Out) extends Single
}
