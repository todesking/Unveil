package com.todesking.unveil

import java.lang.reflect.{ Method => JMethod, Field => JField, Constructor => JConstructor }

object Reflect {
  // TODO: default interface method
  def allJMethods(jClass: Class[_]): Map[(ClassRef, MethodRef), JMethod] =
    supers(jClass)
      .flatMap(_.getDeclaredMethods)
      .map { m => (ClassRef.of(m.getDeclaringClass) -> MethodRef.from(m)) -> m }
      .toMap

  def allJConstructors(jClass: Class[_]): Map[(ClassRef, MethodRef), JConstructor[_]] =
    supers(jClass)
      .flatMap(_.getDeclaredConstructors)
      .map { m => (ClassRef.of(m.getDeclaringClass) -> MethodRef.from(m)) -> m }
      .toMap

  // TODO: default interface method
  def resolveVirtualMethod(jClass: Class[_], mr: MethodRef): ClassRef.Concrete =
    supers(jClass)
      .find { c =>
        c.getDeclaredMethods
          .filter { m => MethodAttribute.from(m.getModifiers).isVirtual }
          .exists { m => MethodRef.from(m) == mr }
      }.map { c => ClassRef.of(c) }
      .getOrElse { throw new IllegalArgumentException(s"Can't find virtual method ${mr} in ${jClass}") }

  // TODO: default interface method
  def virtualJMethods(jClass: Class[_]): Map[MethodRef, JMethod] =
    supers(jClass)
      .reverse
      .flatMap(_.getDeclaredMethods)
      .filterNot { m => MethodAttribute.Private.enabledIn(m.getModifiers) }
      .foldLeft(Map.empty[MethodRef, JMethod]) {
        case (map, m) =>
          map + (MethodRef.from(m) -> m)
      }

  def allJFields(jClass: Class[_]): Map[(ClassRef, FieldRef), JField] =
    supers(jClass)
      .flatMap(_.getDeclaredFields)
      .map { f => f.setAccessible(true); f } // I believe this it no trigger any bad side-effects
      .map { f => (ClassRef.of(f.getDeclaringClass) -> FieldRef.from(f)) -> f }
      .toMap

  def supers(klass: Class[_]): Seq[Class[_]] =
    klass +: Option(klass.getSuperclass).toSeq.flatMap(supers)

  def superClassOf(cr: ClassRef): Option[ClassRef] =
    cr match {
      case cr @ ClassRef.Concrete(_, _) =>
        Option(cr.loadClass.getSuperclass).map(ClassRef.of)
      case ClassRef.Extend(s, _, _, _) =>
        Some(s)
    }
}

