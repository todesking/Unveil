package com.todesking.unveil

sealed abstract class DataPort {
  def name: String
}
object DataPort {
  case class In(override val name: String) extends DataPort {
    override def toString = s"$name(in)"
  }
  case class Out(override val name: String) extends DataPort {
    override def toString = s"$name(out)"
  }
}
