package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.collection.JavaConversions._

import java.lang.reflect.{ Method => JMethod, Field => JField, Modifier, Constructor }

import com.todesking.scalapp.syntax._
import Syntax.Upcast

sealed abstract class Instance[A <: AnyRef] {
  def klass: Klass
  def escaped: Boolean =
    accessibleMethods
      .filter { case (cr, mr) => cr < ClassRef.Object }
      .exists { case (cr, mr) =>
        dataflow(cr, mr).escaped(DataSource.This)
      }
  def toData: Data = Data.reference(this)

  def accessibleMethods: Set[(ClassRef, MethodRef)] =
    methods.filterNot(_._2.isPrivate).keySet

  // TODO: rename virtualMethodBody
  final def methodBody(ref: MethodRef): MethodBody =
    methodBody(resolveVirtualMethod(ref), ref)

  final def methodBody(classRef: ClassRef, methodRef: MethodRef): MethodBody =
    klass.methodBody(classRef, methodRef)

  def fieldValues: Map[(ClassRef, FieldRef), Data]

  def dataflow(methodRef: MethodRef): DataFlow =
    methodBody(methodRef).dataflow(this)

  def dataflow(classRef: ClassRef, methodRef: MethodRef): DataFlow =
    methodBody(classRef, methodRef).dataflow(this)

  def thisRef: ClassRef

  final def methods: Map[(ClassRef, MethodRef), MethodAttribute] =
    klass.instanceMethods

  final def virtualMethods: Map[MethodRef, MethodAttribute] =
    methods.filter(_._2.isVirtual).map { case ((cr, mr), a) => mr -> a }

  final def rewritableVirtualMethods: Map[MethodRef, MethodAttribute] =
    virtualMethods.filterNot(_._2.isFinal).filterNot(_._2.isNative)

  final def fieldKeys: Set[(ClassRef, FieldRef)] =
    klass.instanceFieldAttributes.keySet

  final def resolveVirtualMethod(mr: MethodRef): ClassRef =
    klass.resolveVirtualMethod(mr)

  // TODO: interface field???
  def resolveField(cr: ClassRef, fr: FieldRef): ClassRef =
    klass.resolveInstanceField(cr, fr)

  def valueOption: Option[A]

  def usedMethodsOf(i: Instance[_ <: AnyRef]): Set[(ClassRef, MethodRef)] =
    analyzeMethods(Set.empty[(ClassRef, MethodRef)]) { (agg, cr, mr, df) => agg ++ df.usedMethodsOf(i) }

  def usedFieldsOf(i: Instance[_ <: AnyRef]): Set[(ClassRef, FieldRef)] =
    analyzeMethods(Set.empty[(ClassRef, FieldRef)]) { case (agg, cr, mr, df) => agg ++ df.usedFieldsOf(i) }

  def analyzeMethods[B](initial: B)(analyze: (B, ClassRef, MethodRef, DataFlow) => B): B = {
    // TODO: Exclude overriden and unaccessed method
    val ms = methods.filterNot { case (k, attrs) => attrs.isAbstract }.keys.toSeq.filterNot { case (cr, mr) => cr == ClassRef.Object }
    ms.foldLeft(initial) { case (agg, (cr, mr)) => analyze(agg, cr, mr, dataflow(cr, mr)) }
  }

  def pretty: String
}
object Instance {
  def of[A <: AnyRef](value: A): Original[A] = Original(value)

  sealed abstract class Concrete[A <: AnyRef] extends Instance[A] {
    def materialize(el: EventLogger): Instance.Original[A]
    // TODO: really need it?
    def value: A
    def duplicate[B >: A <: AnyRef: ClassTag](el: EventLogger): Instance[B]
    def duplicate1(el: EventLogger): Instance.Duplicate[A]
    // TODO: change to instanceFields
    def fields: Map[(ClassRef, FieldRef), Field]

    override def fieldValues: Map[(ClassRef, FieldRef), Data.Concrete]
  }
  sealed abstract class Abstract[A <: AnyRef] extends Instance[A] {
    final override def valueOption = None
  }

  case class Original[A <: AnyRef](value: A) extends Concrete[A] {
    require(value != null)

    override def hashCode = System.identityHashCode(value)
    override def equals(rhs: Any) = rhs match {
      case Original(v) => this.value eq v
      case _ => false
    }

    override def valueOption = Some(value)

    override val klass: Klass.Native = Klass.from(value.getClass)

    override def pretty = s"Instance.Original(${klass.name})"

    override def toString = pretty

    override def materialize(el: EventLogger) = this

    override val thisRef: ClassRef.Concrete = klass.ref

    override def duplicate[B >: A <: AnyRef: ClassTag](el: EventLogger): Duplicate[B] =
      el.section("Original.duplicate") { el =>
        duplicate1(el).duplicate[B](el)
      }

    override def duplicate1(el: EventLogger): Duplicate[A] =
      Instance.duplicate(this, thisRef, el)

    override lazy val fields: Map[(ClassRef, FieldRef), Field] =
      klass.instanceFieldAttributes
        .filterNot(_._2.isStatic)
        .map { case (k @ (cr, fr), a) => k -> klass.readField(value, cr, fr) }

    override lazy val fieldValues: Map[(ClassRef, FieldRef), Data.Concrete] =
      fields.mapValues(_.data)
  }

  class Duplicate[A <: AnyRef](
      override val klass: Klass.Modified,
      override val fieldValues: Map[(ClassRef, FieldRef), Data.Concrete]
  ) extends Concrete[A] with Equality.Reference {
    klass.requireWholeInstanceField(fieldValues.keySet)

    override def value = materialize(new EventLogger).value
    override def valueOption = Some(value)

    override def thisRef: ClassRef.Extend =
      klass.ref

    def superRef: ClassRef =
      klass.`super`.ref

    def setFieldValues(vs: Map[(ClassRef, FieldRef), Data.Concrete]): Duplicate[A] = {
      require(vs.keySet subsetOf fields.keySet)

      val (thisValues, superValues) =
        vs.partition { case ((cr, fr), f) => cr == thisRef }

      new Duplicate[A](
        klass,
        fieldValues ++ vs
      )
    }

    override def toString = s"Instance.Duplicate(${thisRef})"
    override def pretty: String =
      klass.pretty // TODO: add field values

    def addMethod(mr: MethodRef, body: MethodBody): Duplicate[A] =
      new Duplicate(klass.addMethod(mr, body), fieldValues)

    def addMethods(ms: Map[MethodRef, MethodBody]): Duplicate[A] =
      ms.foldLeft(this) { case (i, (mr, b)) => i.addMethod(mr, b) }

    private[this] def modifyKlass(f: klass.type => Klass.Modified): Duplicate[A] =
      new Duplicate(f(klass), fieldValues)

    def addField(fr: FieldRef, field: Field): Duplicate[A] = {
      new Duplicate(
        klass.addField(fr, field.attribute),
        fieldValues + ((thisRef, fr) -> field.data)
      )
    }

    def addFields(fs: Map[FieldRef, Field]): Duplicate[A] =
      fs.foldLeft(this) { case (i, (fr, f)) => i.addField(fr, f) }

    override def duplicate1(el: EventLogger) =
      rewriteThisRef(thisRef.anotherUniqueName)

    override def duplicate[B >: A <: AnyRef: ClassTag](el: EventLogger): Duplicate[B] = {
      val newSuperRef = ClassRef.of(implicitly[ClassTag[B]].runtimeClass)
      Instance.duplicate(this, newSuperRef, el)
    }

    // TODO: replace thisRef in method/field
    def rewriteThisRef(newRef: ClassRef.Extend): Duplicate[A] =
      new Duplicate[A](
        klass.changeRef(newRef),
        fieldValues.map {
          case (k @ (cr, fr), v) =>
            if (cr == thisRef) ((newRef -> fr) -> v)
            else k -> v
        }
      )

    override lazy val fields: Map[(ClassRef, FieldRef), Field] =
      klass.instanceFieldAttributes.map {
        case (k @ (cr, fr), fa) =>
          k -> Field(fr.descriptor, fa, fieldValues(k))
      }

    override def materialize(el: EventLogger): Original[A] =
      klass.materialize(fieldValues, el)
        .newInstance[A]()
  }

  class New[A <: AnyRef](override val klass: Klass.Native, constructor: MethodDescriptor) extends Abstract[A] with Equality.Reference {
    def constructorDataFlow: DataFlow =
      dataflow(klass.ref, MethodRef.constructor(constructor))

    override lazy val fieldValues =
      klass.instanceFieldAttributes.map { case (k @ (cr, fr), a) => k -> Data.Unknown(fr.typeRef) }

    override def pretty: String = s"new ${klass.ref}(${constructor.argsStr})"
    override def toString = pretty
    override def thisRef = klass.ref
  }

  class Given[A <: AnyRef](override val klass: Klass, valueOverrides: Map[(ClassRef, FieldRef), Data]) extends Abstract[A] with Equality.Reference {
    override lazy val fieldValues =
      klass.instanceFieldAttributes.map { case (k@(cr, fr), a) =>
        k -> valueOverrides.get(k).getOrElse(Data.Unknown(fr.typeRef))
      }
    override def thisRef = klass.ref
    override def pretty = s"<given instance of ${klass.ref}>"
  }

  private def duplicate[A <: AnyRef, B >: A <: AnyRef](o: Instance.Concrete[A], superRef: ClassRef.Concrete, el: EventLogger): Duplicate[B] = {
    el.section("Instance.duplicate") { el =>
      el.logCFields("base instance fields", o.fields.keySet)
      val (klass, fieldRenaming) = o.klass.duplicate(superRef, el)
      val fieldValues =
        o.fields.flatMap {
          case (k @ (cr, fr), field) =>
            fieldRenaming.get(k).fold {
              if (cr < superRef) Map.empty[(ClassRef, FieldRef), Data.Concrete]
              else Map(k -> field.data)
            } { newFr =>
              if (cr < superRef)
                Map((klass.ref.asInstanceOf[ClassRef] -> newFr) -> field.data)
              else
                Map(
                  k -> field.data,
                  (klass.ref.asInstanceOf[ClassRef] -> newFr) -> field.data
                )
            }
        }
      el.logCFields("valued fields", fieldValues.keys)
      new Duplicate[B](
        klass,
        fieldValues
      )
    }
  }

  // TODO: Weaken CL
  private[this] val materializedClasses = mutable.HashMap.empty[(ClassLoader, String), Array[Byte]]
  def registerMaterialized(cl: ClassLoader, name: String, bytes: Array[Byte]): Unit = synchronized {
    if (materializedClasses.contains(cl -> name))
      throw new IllegalArgumentException(s"${name} is already defined in ${cl}")
    materializedClasses(cl -> name) = bytes
  }
  // TODO: Resolve name conflict
  def findMaterializedClasses(cl: ClassLoader): Seq[(String, Array[Byte])] = synchronized {
    if (cl == null) {
      Seq.empty
    } else {
      materializedClasses.collect { case ((l, n), b) if l == cl => (n -> b) }.toSeq ++
        findMaterializedClasses(cl.getParent)
    }
  }
}

