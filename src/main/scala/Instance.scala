package com.todesking.unveil

import scala.language.existentials
import scala.language.higherKinds

import scala.reflect.{ classTag, ClassTag }
import scala.collection.mutable
import scala.collection.JavaConversions._

import java.lang.reflect.{ Method => JMethod, Field => JField, Modifier, Constructor }

import com.todesking.scalapp.syntax._

sealed abstract class Instance[A <: AnyRef] {
  // TODO: rename virtualMethodBody
  def methodBody(ref: MethodRef): MethodBody =
    methodBody(resolveVirtualMethod(ref), ref)

  def methodBody(classRef: ClassRef, methodRef: MethodRef): MethodBody

  def dataflow(methodRef: MethodRef): DataFlow =
    methodBody(methodRef).dataflow(this)

  def dataflow(classRef: ClassRef, methodRef: MethodRef): DataFlow =
    methodBody(classRef, methodRef).dataflow(this)

  def thisRef: ClassRef

  // TODO: change to instanceMethods
  def methods: Map[(ClassRef, MethodRef), MethodAttribute]

  def virtualMethods: Map[MethodRef, MethodAttribute] =
    methods.filter(_._2.isVirtual).map { case ((cr, mr), a) => mr -> a }

  def rewritableVirtualMethods: Map[MethodRef, MethodAttribute] =
    virtualMethods.filterNot(_._2.isFinal).filterNot(_._2.isNative)

  // TODO: change to instanceFields
  def fields: Map[(ClassRef, FieldRef), Field]

  def hasVirtualMethod(ref: MethodRef): Boolean =
    methods.exists { case ((c, m), a) => m == ref && a.isVirtual }

  def hasVirtualMethod(ref: String): Boolean =
    hasVirtualMethod(MethodRef.parse(ref, thisRef.classLoader))

  def extendMethods(seed: Set[(ClassRef, MethodRef)]): Set[(ClassRef, MethodRef)] = {
    var ms: Set[(ClassRef, MethodRef)] = null
    var ext: Set[(ClassRef, MethodRef)] = seed
    do {
      ms = ext
      ext = ms ++ ms.flatMap { case (cr, mr) => dataflow(cr, mr).usedMethodsOf(this) }
    } while (ext.size > ms.size)
    ext
  }

  def resolveVirtualMethod(mr: MethodRef): ClassRef

  def materialized: Instance.Original[A]

  def duplicate[B >: A <: AnyRef: ClassTag]: Instance.Duplicate[B]

  def duplicate1: Instance.Duplicate[A]

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

  case class Original[A <: AnyRef](value: A) extends Instance[A] {
    require(value != null)

    override final def hashCode() =
      System.identityHashCode(value)

    override final def equals(that: Any) = that match {
      case Original(v) => value eq v
      case _ => false
    }

    override def pretty = s"Original(${jClass.getName})"

    override def toString = s"Instance.Original(${jClass.getName})"

    override def materialized = this

    override val thisRef = ClassRef.of(jClass)

    override def resolveVirtualMethod(mr: MethodRef): ClassRef =
      Reflect.resolveVirtualMethod(jClass, mr)

    override def duplicate[B >: A <: AnyRef: ClassTag]: Duplicate[B] =
      duplicate1.duplicate[B]

    override def duplicate1: Duplicate[A] =
      Duplicate[A](
        this,
        thisRef.extend(thisRef.classLoader),
        methods,
        Map.empty,
        fields,
        Map.empty,
        Map.empty
      )

    override def methodBody(cr: ClassRef, mr: MethodRef) =
      if (mr.isInit) MethodBody.parse(allJConstructors(cr -> mr))
      else MethodBody.parse(allJMethods(cr -> mr))

    override lazy val methods: Map[(ClassRef, MethodRef), MethodAttribute] =
      allJMethods.map { case (k, m) => k -> MethodAttribute.from(m) }.filterNot(_._2.isStatic)

    override lazy val fields: Map[(ClassRef, FieldRef), Field] =
      allJFields.map { case ((cr, fr), f) => (cr -> fr) -> Field.from(f, value) }.filterNot(_._2.attribute.isStatic)

    private[this] lazy val jClass = value.getClass
    private[this] lazy val virtualJMethods = Reflect.virtualJMethods(jClass)
    private[this] lazy val allJMethods = Reflect.allJMethods(jClass)
    private[this] lazy val allJFields = Reflect.allJFields(jClass)
    private[this] lazy val allJConstructors = Reflect.allJConstructors(jClass)
  }

  case class Duplicate[A <: AnyRef](
      orig: Original[_ <: A],
      override val thisRef: ClassRef.Extend,
      superMethods: Map[(ClassRef, MethodRef), MethodAttribute],
      thisMethods: Map[MethodRef, MethodBody],
      superFields: Map[(ClassRef, FieldRef), Field], // super class field values
      thisFields: Map[FieldRef, Field],
      fieldValues: Map[(ClassRef, FieldRef), Field]
  ) extends Instance[A] {
    override final def hashCode() =
      System.identityHashCode(this)

    override final def equals(that: Any) = that match {
      case that: Duplicate[_] => this eq that
      case _ => false
    }

    def setFieldValues(vs: Map[(ClassRef, FieldRef), Field]): Duplicate[A] = {
      require(vs.keySet subsetOf fields.keySet)

      val (thisValues, superValues) =
        vs.partition { case ((cr, fr), f) => cr == thisRef }

      copy(
        fieldValues = fieldValues ++ superValues,
        thisFields = thisFields ++ thisValues.map { case ((cr, fr), f) => (fr -> f) }
      )
    }

    override def toString = s"Instance.Duplicate(${thisRef})"
    override def pretty: String = s"""class ${thisRef}
constructor:
${constructorBody.descriptor}
${
      constructorBody.pretty
    }
new/overriden methods:
${
      thisMethods.map {
        case (mr, body) =>
          s"""def ${mr} ${body.attribute}
${body.pretty}"""
      }.mkString("\n")
    }
New fields:
${
      thisFields.map {
        case (fr, field) => s"$fr ${field.attribute}"
      }.mkString("\n")
    }
Super fields:
${
      superFields.map {
        case ((cr, fr), f) => s"$cr.$fr ${f.attribute}"
      }.mkString("\n")
    }
"""

    def addMethod(mr: MethodRef, body: MethodBody): Duplicate[A] = {
      require(mr.descriptor == body.descriptor)
      require(!body.attribute.isStatic)
      copy(thisMethods = thisMethods + (mr -> body))
    }

    def addMethods(ms: Map[MethodRef, MethodBody]): Duplicate[A] =
      ms.foldLeft(this) { case (i, (mr, b)) => i.addMethod(mr, b) }

    def addField(fr: FieldRef, field: Field): Duplicate[A] = {
      require(!field.attribute.isStatic)
      copy(thisFields = thisFields + (fr -> field))
    }

    def addFields(fs: Map[FieldRef, Field]): Duplicate[A] =
      fs.foldLeft(this) { case (i, (fr, f)) => i.addField(fr, f) }

    // TODO: default interface method
    override def resolveVirtualMethod(mr: MethodRef): ClassRef = {
      thisMethods.get(mr).map { body =>
        if (body.attribute.isVirtual) thisRef
        else throw new IllegalArgumentException(s"Not virtual: ${mr} ${body.attribute}")
      } getOrElse {
        orig.resolveVirtualMethod(mr)
      }
    }

    override def duplicate1 =
      rewriteThisRef(thisRef.anotherUniqueName)

    override def duplicate[B >: A <: AnyRef: ClassTag]: Duplicate[B] = {
      val newSuperRef = ClassRef.of(implicitly[ClassTag[B]].runtimeClass)
      val newRef = newSuperRef.extend(thisRef.anotherUniqueName.name, thisRef.classLoader)
      val fieldMappings: Map[(ClassRef, FieldRef), FieldRef] =
        superFields
          .keys
          .filter { case (c, f) => c < newSuperRef }
          .map { case (c, f) => (c, f) -> f.anotherUniqueName(f.name) }
          .toMap
      val newThisFields =
        superFields
          .flatMap {
            case (cf, field) =>
              fieldMappings.get(cf).map { fr =>
                fr -> field.copy(attribute = field.attribute.makePrivate)
              }
          }
      // TODO: support super methods
      val newThisMethods =
        superMethods
          .filter { case ((cr, mr), ma) => cr < newSuperRef }
          .map {
            case ((cr, mr), ma) =>
              import Bytecode._
              val body = orig.methodBody(cr, mr)
              mr -> body.rewrite {
                case iv @ invokevirtual(cref, mref) if cref < newSuperRef =>
                  invokevirtual(newRef, mref)
                case bc @ getfield(cref, fref) if fieldMappings.contains(cref -> fref) =>
                  getfield(newRef, fieldMappings(cref -> fref))
                case bc @ putfield(cref, fref) if fieldMappings.contains(cref -> fref) =>
                  putfield(newRef, fieldMappings(cref -> fref))
              }
          }
      copy[B](
        superMethods = superMethods.filterNot { case ((cr, mr), ma) => cr < newSuperRef },
        superFields = superFields.filterNot { case ((cr, fr), f) => cr < newSuperRef },
        thisMethods = thisMethods ++ newThisMethods,
        thisFields = thisFields ++ newThisFields
      ).rewriteThisRef(newRef)
    }

    // TODO: should we replace thisRef in method/field signature?
    // TODO: should we replace only if objectref == this ?
    def rewriteThisRef(newRef: ClassRef.Extend): Duplicate[A] =
      copy(
        thisRef = newRef,
        thisMethods = thisMethods.map { case (ref, body) => ref -> body.rewriteClassRef(thisRef, newRef) }
      )

    override def methodBody(cr: ClassRef, mr: MethodRef) =
      if (cr == thisRef) thisMethods(mr)
      else if (thisRef < cr) orig.methodBody(cr, mr)
      else throw new IllegalArgumentException(s"Method not found: ${cr.pretty}.${mr.str}")

    override def methods =
      superMethods ++ thisMethods.map { case (k, v) => (thisRef -> k) -> v.attribute }

    override lazy val fields: Map[(ClassRef, FieldRef), Field] =
      superFields.map {
        case ((cr, fr), f) => (cr -> fr) -> fieldValues.get(cr -> fr).getOrElse(f)
      } ++ thisFields.map { case (fref, f) => ((thisRef -> fref) -> f) }

    def superClass: Class[_] = thisRef.superClassRef.loadClass

    lazy val thisFieldsSeq: Seq[(FieldRef, Field)] = thisFields.toSeq
    lazy val superConstructor: Analyze.SetterConstructor =
      Analyze.findSetterConstructor(this, superClass, superFields) getOrElse {
        throw new TransformException(s"Usable constructor not found")
      }
    lazy val superConstructorArgs: Seq[Any] = superConstructor.toArguments(superFields)
    lazy val constructorArgs: Seq[(TypeRef.Public, Any)] =
      thisFieldsSeq
        .map { case (r, f) => (f.descriptor.typeRef -> f.data.concreteValue) } ++
        superConstructor.descriptor.args.zip(superConstructorArgs)

    lazy val constructorDescriptor = MethodDescriptor(TypeRef.Void, constructorArgs.map(_._1))
    lazy val constructorBody: MethodBody = {
      val thisFieldAssigns: Seq[(FieldRef, Int)] =
        thisFieldsSeq.zipWithIndex.map { case ((fr, f), i) => fr -> (i + 1) }
      import Bytecode._
      MethodBody(
        descriptor = constructorDescriptor,
        MethodAttribute.Public,
        jumpTargets = Map.empty,
        bytecode =
        Seq(
          Seq(aload(0)),
          superConstructor.descriptor.args.zipWithIndex.map {
            case (t, i) =>
              load(t, i + thisFieldAssigns.size + 1)
          },
          Seq(
            invokespecial(
              ClassRef.of(superClass),
              superConstructor.methodRef
            )
          )
        ).flatten ++ thisFieldAssigns.flatMap {
            case (fr, i) =>
              import Bytecode._
              Seq(
                aload(0),
                load(fr.descriptor.typeRef, i),
                putfield(thisRef, fr)
              )
          }.toSeq ++ Seq(vreturn())
      )
    }
    override lazy val materialized: Original[A] = {
      import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod, CtField, CtConstructor, ByteArrayClassPath }
      import javassist.bytecode.{ Bytecode => JABytecode, MethodInfo }

      import Javassist.ctClass

      validate()

      val superClass = thisRef.superClassRef.loadClass
      val classLoader = thisRef.classLoader

      val classPool = new ClassPool(null)
      Instance.findMaterializedClasses(classLoader).foreach {
        case (name, bytes) =>
          classPool.appendClassPath(new ByteArrayClassPath(name, bytes))
      }
      classPool.appendClassPath(new ClassClassPath(superClass))

      val ctBase = classPool.get(superClass.getName)

      val klass = classPool.makeClass(thisRef.name, ctBase)
      val constPool = klass.getClassFile.getConstPool
      val ctObject = classPool.get("java.lang.Object")
      import Bytecode._
      thisMethods
        .foreach {
          case (ref, body) =>
            val codeAttribute = Javassist.compile(classPool, constPool, body.dataflow(this))
            val minfo = new MethodInfo(constPool, ref.name, ref.descriptor.str)
            minfo.setCodeAttribute(codeAttribute)
            val sm = javassist.bytecode.stackmap.MapMaker.make(classPool, minfo)
            codeAttribute.setAttribute(sm)
            klass.getClassFile.addMethod(minfo)
        }

      thisFields.foreach {
        case (ref, field) =>
          val ctf = new CtField(ctClass(ref.descriptor.typeRef), ref.name, klass)
          ctf.setModifiers(field.attribute.toInt)
          klass.addField(ctf)
      }

      val ctor = new CtConstructor(constructorArgs.map(_._1).map(ctClass).toArray, klass)
      klass.addConstructor(ctor)

      val ctorMethodInfo =
        klass
          .getClassFile
          .getMethods
          .map(_.asInstanceOf[MethodInfo])
          .find(_.getName == "<init>")
          .get

      val ctorCA = Javassist.compile(classPool, constPool, constructorBody.dataflow(this))
      ctorMethodInfo.setCodeAttribute(ctorCA)
      val sm = javassist.bytecode.stackmap.MapMaker.make(classPool, ctorMethodInfo)
      ctorCA.setAttribute(sm)

      val concreteClass = klass.toClass(classLoader, null)
      val value =
        try {
          concreteClass
            .getDeclaredConstructor(constructorArgs.map(_._1.javaClass).toArray: _*)
            .newInstance(constructorArgs.map(_._2.asInstanceOf[Object]).toArray: _*)
            .asInstanceOf[A]
        } catch {
          case e: LinkageError => throw new InvalidClassException(this, e)
        }
      val bytes = klass.toBytecode
      Instance.registerMaterialized(classLoader, klass.getName, bytes)
      Instance.of(value)
    }

    private[this] def validate(): Unit = {
      def fail(msg: String) =
        throw new IllegalStateException(msg)

      if ((thisRef.superClassRef.loadClass.getModifiers & Modifier.FINAL) == Modifier.FINAL)
        fail("base is final class")
      // TODO: check finalizer
      // * for each fields `f` in `x`:
      //   * FAIL if `f` is non-final and `x` is _escaped_
      //   * if `f` defined at `_ <<: X`
      //     * FAIL if
      //       * `f` has type `_ <<: X`
      // * for each ALL methods/constructors `m` in `x`:
      //   * FAIL if
      //     * `m` is abstract
      //     * `m` takes parameter `_ <<: X`
      //     * `m` returns `_ <<: X`
      //     * `m` has non-this reference `_ <<: X`
      // * for each visible or self-referenced non-constructor methods `m` in `x`:
      //   * if `m` defined at `_ <<: X`
      //     * FAIL if
      //       * `m` is native
      //       * `m` leaks `this` as `_ <<: X`
      // * for each constructor/used super constructor `c` in `x`:
      //   * FAIL if ANY OF
      //     * `c` is native
      //     * `c` may have side-effect
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

