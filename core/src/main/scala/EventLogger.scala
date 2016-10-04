package com.todesking.unveil

import scala.language.existentials

import scala.collection.mutable

import EventLogger.{ Event, Path }

class EventLogger {
  private[this] val eventBuffer = mutable.ArrayBuffer.empty[Event]

  private[this] def withSubEL[A](path: Path)(f: EventLogger => A): A = {
    val el = new EventLogger
    val ret =
      try {
        f(el)
      } catch {
        case e: Throwable =>
          el.fail(e)
          eventBuffer += Event.Grouped(path, el.events)
          throw e
      }
    eventBuffer += Event.Grouped(path, el.events)
    ret
  }

  def clear(): Unit = eventBuffer.clear()

  def events: Seq[Event] = eventBuffer.toSeq

  def apply[A](f: EventLogger => A): A =
    try {
      f(this)
    } catch {
      case e: Throwable =>
        eventBuffer += Event.Fail(e)
        throw e
    }

  def enterField[A](cr: ClassRef, fr: FieldRef)(f: EventLogger => A): A =
    withSubEL(Path.Field(cr, fr))(f)
  def enterMethod[A](cr: ClassRef, mr: MethodRef)(f: EventLogger => A): A =
    withSubEL(Path.Method(cr, mr))(f)

  def enterTransformer[A](t: Transformer, i: Instance[_ <: AnyRef])(f: EventLogger => A): A =
    withSubEL(Path.Transformer(t, i))(f)

  def section[A](title: String)(f: EventLogger => A): A =
    withSubEL(Path.Section(title))(f)

  def log(msg: String): Unit =
    eventBuffer += Event.Message(msg)

  def logCFields(desc: String, cfs: Iterable[(ClassRef, FieldRef)]): Unit =
    eventBuffer += Event.CFields(desc, cfs.toSeq)

  def logCMethods(desc: String, cms: Iterable[(ClassRef, MethodRef)]): Unit =
    eventBuffer += Event.CMethods(desc, cms.toSeq)

  def logMethods(desc: String, ms: Iterable[MethodRef]): Unit =
    eventBuffer += Event.Methods(desc, ms.toSeq)

  def logFields(desc: String, fs: Iterable[FieldRef]): Unit =
    eventBuffer += Event.Fields(desc, fs.toSeq)

  def logFieldValues(desc: String, fvs: Iterable[((ClassRef, FieldRef), Data)]): Unit =
    eventBuffer += Event.FieldValues(desc, fvs.toSeq)

  def fail(e: Throwable): Unit =
    eventBuffer += Event.Fail(e)

  def pretty(): String = pretty(0, events).split("\n").map { line =>
    val indent = (line.size - line.replaceAll("^ +", "").size) / 2
    f"$indent%1X$line"
  }.mkString("\n")

  private[this] def pretty(indent: Int, evs: Seq[Event]): String = {
    evs
      .map(prettyEvent)
      .flatMap(_.split("\n"))
      .map { s => "  " * indent + s }
      .mkString("\n")
  }

  // TODO[refactor]: fields/methods
  private[this] def prettyEvent(event: Event): String = event match {
    case Event.Message(msg) =>
      msg
    case Event.Fail(e) =>
      s"FAIL: $e"
    case Event.Grouped(path, evs) =>
      prettyPath(path) + "\n" + pretty(1, evs)
    case Event.CFields(desc, cfs) =>
      s"$desc =" + (
        if (cfs.isEmpty) ""
        else cfs.map { case (cr, fr) => s"- $cr\n    .$fr" }.mkString("\n", "\n", "")
      )
    case Event.CMethods(desc, cms) =>
      s"$desc =" + (
        if (cms.isEmpty) ""
        else cms.map { case (cr, mr) => s"- $cr\n    .$mr" }.mkString("\n", "\n", "")
      )
    case Event.Methods(desc, ms) =>
      s"$desc =" + (
        if (ms.isEmpty) ""
        else ms.map { case mr => s"- $mr" }.mkString("\n", "\n", "")
      )
    case Event.Fields(desc, fs) =>
      s"$desc =" + (
        if (fs.isEmpty) ""
        else fs.map { case fr => s"- $fr" }.mkString("\n", "\n", "")
      )
    case Event.FieldValues(desc, fvs) =>
      s"$desc =" + (
        if (fvs.isEmpty) ""
        else fvs.map { case ((cr, fr), v) => s"- $cr\n     .$fr = $v" }.mkString("\n", "\n", "")
      )
  }

  private[this] def prettyPath(path: Path) = path match {
    case Path.Section(title) =>
      s"SECTION: $title"
    case Path.Field(cr, fr) =>
      s"""ENTERING FIELD: ${fr.name}
      |  class = $cr
      |  field = $fr""".stripMargin('|')
    case Path.Method(cr, mr) =>
      s"""ENTERING METHOD: ${mr.name}
      |  class = $cr
      |  method = $mr""".stripMargin('|')
    case Path.Transformer(t, i) =>
      s"""APPLYING TRANSFORMER: ${t.name}""" + (
        if (t.params.isEmpty) ""
        else t.params.map { case (k, v) => s"  $k = $v" }.mkString("\n", "\n", "")
      ) + s"\n  instance = ${i.thisRef}"
  }
}

object EventLogger {
  sealed abstract class Path
  object Path {
    case class Field(classRef: ClassRef, fieldRef: FieldRef) extends Path
    case class Method(classRef: ClassRef, methodRef: MethodRef) extends Path
    case class Transformer(transformer: com.todesking.unveil.Transformer, instance: Instance[_ <: AnyRef]) extends Path
    case class Section(title: String) extends Path
  }
  sealed abstract class Event
  object Event {
    case class Message(message: String) extends Event
    case class Fail(e: Throwable) extends Event
    case class Grouped(path: Path, events: Seq[Event]) extends Event
    case class CFields(desc: String, cfs: Seq[(ClassRef, FieldRef)]) extends Event
    case class CMethods(desc: String, cfs: Seq[(ClassRef, MethodRef)]) extends Event
    case class Methods(desc: String, ms: Seq[MethodRef]) extends Event
    case class Fields(desc: String, fs: Seq[FieldRef]) extends Event
    case class FieldValues(desc: String, fvs: Seq[((ClassRef, FieldRef), Data)]) extends Event
  }

}
