package com.todesking.unveil

sealed abstract class DataSource {
  def merge(rhs: DataSource): DataSource
  def may(rhs: DataSource.Single): Boolean
}
object DataSource {
  case class Multiple(sources: Set[DataSource.Single]) extends DataSource {
    override def merge(rhs: DataSource): DataSource = rhs match {
      case Multiple(ss) => Multiple(sources ++ ss)
      case s: Single => Multiple(sources + s)
    }
    override def may(rhs: DataSource.Single): Boolean =
      sources.contains(rhs)
  }

  sealed abstract class Single extends DataSource {
    override def merge(rhs: DataSource): DataSource = rhs match {
      case Multiple(sources) => Multiple(sources + this)
      case s: Single => Multiple(Set(this, s))
    }
    override def may(rhs: DataSource.Single): Boolean =
      this == rhs
  }

  case object This extends Single
  case class Argument(n: Int) extends Single
  case class Field(classRef: ClassRef, fieldRef: FieldRef) extends Single
  import com.todesking.unveil.{Bytecode => TopBytecode}
  case class Bytecode(label: TopBytecode.Label, port: DataPort.Out) extends Single
}
