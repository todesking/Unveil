package com.todesking.unveil

sealed abstract class DataSource {
  def merge(rhs: DataSource): DataSource
}
object DataSource {
  import com.todesking.unveil.{Bytecode => TopBytecode}
  case class Multiple(sources: Set[DataSource.Single]) extends DataSource {
    override def merge(rhs: DataSource): DataSource = rhs match {
      case Multiple(ss) => Multiple(sources ++ ss)
      case s: Single => Multiple(sources + s)
    }
  }

  sealed abstract class Single extends DataSource {
    override def merge(rhs: DataSource): DataSource = rhs match {
      case Multiple(sources) => Multiple(sources + this)
      case s: Single => Multiple(Set(this, s))
    }
  }

  case object This extends Single
  case class Argument(n: Int) extends Single
  case class Field(classRef: ClassRef, fieldRef: FieldRef) extends Single
  case class Bytecode(label: TopBytecode.Label, bytecode: TopBytecode) extends Single
}
