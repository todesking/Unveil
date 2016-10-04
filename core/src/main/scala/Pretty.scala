package com.todesking.unveil

object Pretty {
  def format_MethodBody(mb: MethodBody): String = ???
  def format_DataFlow(df: DataFlow): String = ???
  def format_DataFlow_SSA(ssa: DataFlow.SSA) = ???

  def format_Instance_Original[A <: AnyRef](o: Instance.Original[A]): String = ???
  def format_Instance_Duplicate[A <: AnyRef](dup: Instance.Duplicate[A]): String = ???
  def format_Instance_New[A <: AnyRef](n: Instance.New[A]): String = ???

  def format_Klass(klass: Klass): String = klass match {
    case k: Klass.MaterializedNative => format_Klass_MaterializedNative(k)
    case k: Klass.Modified => format_Klass_Modified(k)
    case k => k.toString
  }
  def format_Klass_MaterializedNative(klass: Klass.MaterializedNative): String = ???
  def format_Klass_Modified(klass: Klass.Modified): String = {
    s"""class ${klass.ref} {
  // new/overriden methods:
${
      klass.declaredMethods.map {
        case (mr, body) =>
          try {
            val df = klass.dataflow(klass.ref, mr)
            s"""  def ${mr} ${body.attribute}
${df.pretty.split("\n").map("    " + _).mkString("\n")}"""
          } catch {
            case scala.util.control.NonFatal(e) =>
              s"""(dataflow analysis failed: $e)
  def ${mr} ${body.attribute}
${body.pretty.split("\n").map("    " + _).mkString("\n")}"""
          }
      }.mkString("\n")
    }
  // New fields:
${
      klass.declaredFields.map {
        case (fr, attr) => s"$fr $attr"
      }.map("  " + _).mkString("\n")
    }
  // Super fields:
${
      klass.fieldAttributes.filterNot(_._1._1 == klass.ref).map {
        case ((cr, fr), attr) => s"$cr.$fr ${attr}"
      }.mkString("\n")
    }
}
"""
  }

  // TODO: super class information
  private[this] def format_Klass0(
    classRef: ClassRef,
    declaredMethods: Map[MethodRef, MethodBody],
    declaredFields: Map[FieldRef, FieldAttribute],
    constructor: Option[(MethodDescriptor, Option[Seq[Data.Concrete]])], // (ctor, args?)?
    fieldValues: Map[(ClassRef, FieldRef), Data]
  ): String = {
    ???
  }
}
