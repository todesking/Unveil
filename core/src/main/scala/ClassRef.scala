package com.todesking.unveil

import scala.language.existentials

import java.util.Objects

sealed abstract class ClassRef {
  def pretty: String
  override def toString = pretty
  def name: String
  def classLoader: ClassLoader
  def binaryName: String = name.replaceAll("\\.", "/")

  def <(rhs: ClassRef): Boolean =
    ClassRef.compare(this, rhs).map { case -1 => true; case 0 => false; case 1 => false } getOrElse false

  def >(rhs: ClassRef): Boolean =
    ClassRef.compare(this, rhs).map { case -1 => false; case 0 => false; case 1 => true } getOrElse false

  def >=(rhs: ClassRef): Boolean =
    this > rhs || this == rhs

  def <=(rhs: ClassRef): Boolean =
    this < rhs || this == rhs

  def toTypeRef: TypeRef.Reference = TypeRef.Reference(this)
  def renamed(newName: String): ClassRef

  override def hashCode = Objects.hashCode(name) ^ Objects.hashCode(classLoader)

  override def equals(obj: Any) =
    obj match {
      case that: ClassRef =>
        this.name == that.name && this.classLoader == that.classLoader
      case _ =>
        false
    }
}
object ClassRef {
  // Some(n): Determinable
  // None: Not sure
  def compare(lhs: ClassRef, rhs: ClassRef): Option[Int] = (lhs, rhs) match {
    case (l: Concrete, r: Concrete) =>
      if (l.loadClass == r.loadClass) Some(0)
      else if (l.loadClass.isAssignableFrom(r.loadClass)) Some(1)
      else if (r.loadClass.isAssignableFrom(l.loadClass)) Some(-1)
      else None
    case (l: Concrete, r: Extend) =>
      if (l.loadClass.isAssignableFrom(r.superClassRef.loadClass)) Some(1)
      else None
    case (l: Extend, r: Concrete) =>
      if (r.loadClass.isAssignableFrom(l.superClassRef.loadClass)) Some(-1)
      else None
    case (l: Extend, r: Extend) =>
      None
  }

  val Object: ClassRef.Concrete = of(classOf[java.lang.Object])

  case class Concrete(override val name: String, override val classLoader: ClassLoader) extends ClassRef {
    override def pretty = s"${name}@${System.identityHashCode(classLoader)}"
    // TODO: Is this really correct?
    lazy val loadClass: Class[_] =
      (if (classLoader == null) ClassLoader.getSystemClassLoader else classLoader).loadClass(name)

    def extend(name: String, cl: AccessibleClassLoader): Extend = {
      if (loadClass.isInterface) Extend(ClassRef.Object, name, cl, Seq(loadClass))
      else Extend(this, name, cl, Seq.empty)
    }

    // TODO: preserve package name
    def extend(cl: AccessibleClassLoader): Extend =
      extend(uniqueNamer("generated"), cl)

    override def renamed(newName: String): Concrete =
      copy(name = newName)
  }

  // TODO: interface
  case class Extend(
      superClassRef: ClassRef.Concrete,
      override val name: String,
      override val classLoader: AccessibleClassLoader,
      interfaces: Seq[Class[_]]
  ) extends ClassRef {
    override def pretty = s"${name}(<:${superClassRef.name} ${interfaces.map(_.getName).mkString(", ")})@${System.identityHashCode(classLoader)}"
    def anotherUniqueName: Extend =
      copy(name = uniqueNamer(name))
    override def renamed(newName: String): Extend =
      copy(name = newName)
  }

  def of(klass: Class[_]): Concrete =
    ClassRef.Concrete(klass.getName, klass.getClassLoader)

  def of(name: String, cl: ClassLoader): Concrete =
    of((if (cl == null) ClassLoader.getSystemClassLoader else cl).loadClass(name))

  private[this] val uniqueNamer = new UniqueNamer
}
