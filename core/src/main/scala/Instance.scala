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

  // TODO: rename this method
  // TODO: [BUG] if `this` is leaked...??
  def extendMethods(seed: Set[(ClassRef, MethodRef)]): Set[(ClassRef, MethodRef)] = {
    // TODO: this is very inefficient
    var ms: Set[(ClassRef, MethodRef)] = null
    var ext: Set[(ClassRef, MethodRef)] = seed
    do {
      ms = ext
      ext = ms ++
        ms.filter { case (cr, _) => cr < ClassRef.Object }
        .flatMap { case (cr, mr) => dataflow(cr, mr).usedMethodsOf(this) }
    } while (ext.size > ms.size)
    ext
  }

  def resolveVirtualMethod(mr: MethodRef): ClassRef

  // TODO: interface field???
  def resolveField(cr: ClassRef, fr: FieldRef): ClassRef =
    if (fields.contains(cr -> fr)) cr
    else Reflect.superClassOf(cr).map { sc => resolveField(sc, fr) } getOrElse {
      println(s"!!!!!!!!!!!!!!!!!!!!! ${fields} !!!!!!!!!!!!!!!!!!!!")
      throw new IllegalArgumentException(s"Field resolution failed: $cr.$fr")
    }

  def materialized: Instance.Original[A]

  def duplicate[B >: A <: AnyRef: ClassTag](el: EventLogger): Instance.Duplicate[B]

  def duplicate1(el: EventLogger): Instance.Duplicate[A]

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

  def methodSSA(cr: ClassRef, mr: MethodRef): DataFlow.SSA = ???
}
object Instance {
  def of[A <: AnyRef](value: A): Original[A] = Original(value)

  case class Original[A <: AnyRef](value: A) extends Instance[A] {
    require(value != null)

    override final def hashCode() =
      System.identityHashCode(value)

    override final def equals(that: Any) = that match {
      case Original(v) => value eq v
      case that: AnyRef => this eq that
    }

    override def pretty = s"Original(${jClass.getName})"

    override def toString = s"Instance.Original(${jClass.getName})"

    override def materialized = this

    override val thisRef = ClassRef.of(jClass)

    override def resolveVirtualMethod(mr: MethodRef): ClassRef =
      Reflect.resolveVirtualMethod(jClass, mr)

    override def duplicate[B >: A <: AnyRef: ClassTag](el: EventLogger): Duplicate[B] =
      el.section("Original.duplicate") { el =>
        duplicate1(el).duplicate[B](el)
      }

    override def duplicate1(el: EventLogger): Duplicate[A] =
      Instance.duplicate(this, this, thisRef, el)

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
      thisMethods: Map[MethodRef, MethodBody],
      thisFields: Map[FieldRef, FieldAttribute],
      fieldValues: Map[(ClassRef, FieldRef), Data.Concrete]
  ) extends Instance[A] {
    override final def hashCode() =
      System.identityHashCode(this)

    override final def equals(that: Any) = that match {
      case that: Duplicate[_] => this eq that
      case _ => false
    }

    def superRef: ClassRef.Concrete =
      thisRef.superClassRef

    def setFieldValues(vs: Map[(ClassRef, FieldRef), Data.Concrete]): Duplicate[A] = {
      require(vs.keySet subsetOf fields.keySet)

      val (thisValues, superValues) =
        vs.partition { case ((cr, fr), f) => cr == thisRef }

      copy(
        fieldValues = fieldValues ++ vs
      )
    }

    override def toString = s"Instance.Duplicate(${thisRef})"
    override def pretty: String = {
      s"""class ${thisRef}
constructor:
${constructorBody.descriptor}
${
        constructorBody.pretty
      }
new/overriden methods:
${
        thisMethods.map {
          case (mr, body) =>
            try {
              val df = dataflow(mr)
              s"""def ${mr} ${body.attribute}
${df.pretty.split("\n").map("  " + _).mkString("\n")}"""
            } catch {
              case scala.util.control.NonFatal(e) =>
                s"""(dataflow analysis failed: $e)
def ${mr} ${body.attribute}
${body.pretty.split("\n").map("  " + _).mkString("\n")}"""
            }
        }.mkString("\n")
      }
New fields:
${
        thisFields.map {
          case (fr, attr) => s"$fr $attr"
        }.mkString("\n")
      }
Super fields:
${
        fields.filterNot(_._1._1 == thisRef).map {
          case ((cr, fr), attr) => s"$cr.$fr ${attr}"
        }.mkString("\n")
      }
"""
    }

    def addMethod(mr: MethodRef, body: MethodBody): Duplicate[A] = {
      require(mr.descriptor == body.descriptor)
      require(!body.attribute.isStatic)
      copy(thisMethods = thisMethods + (mr -> body))
    }

    def addMethods(ms: Map[MethodRef, MethodBody]): Duplicate[A] =
      ms.foldLeft(this) { case (i, (mr, b)) => i.addMethod(mr, b) }

    def addField(fr: FieldRef, field: Field): Duplicate[A] = {
      require(!field.attribute.isStatic)
      copy(
        thisFields = thisFields + (fr -> field.attribute),
        fieldValues = fieldValues + ((thisRef.upcast[ClassRef] -> fr) -> field.data)
      )
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

    override def duplicate1(el: EventLogger) =
      rewriteThisRef(thisRef.anotherUniqueName)

    override def duplicate[B >: A <: AnyRef: ClassTag](el: EventLogger): Duplicate[B] = {
      val newSuperRef = ClassRef.of(implicitly[ClassTag[B]].runtimeClass)
      Instance.duplicate(this, orig, newSuperRef, el)
    }

    // TODO: should we replace thisRef in method/field signature?
    def rewriteThisRef(newRef: ClassRef.Extend): Duplicate[A] =
      copy(
        thisRef = newRef,
        thisMethods = thisMethods.map { case (ref, body) => ref -> body.rewriteClassRef(thisRef, newRef) },
        fieldValues = fieldValues.map {
          case (k @ (cr, fr), v) =>
            if (cr == thisRef) ((newRef -> fr) -> v)
            else k -> v
        }
      )

    override def methodBody(cr: ClassRef, mr: MethodRef) =
      if (cr == thisRef) thisMethods(mr)
      else if (thisRef < cr) orig.methodBody(cr, mr)
      else throw new IllegalArgumentException(s"Method not found: ${cr.pretty}.${mr.str}")

    lazy val superMethods = orig.methods.filter { case ((cr, mr), _) => cr >= superRef }
    lazy val superFields = orig.fields.filter { case ((cr, _), _) => cr >= superRef }

    override lazy val methods =
      superMethods ++ thisMethods.map { case (k, v) => (thisRef -> k) -> v.attribute }

    override lazy val fields: Map[(ClassRef, FieldRef), Field] =
      superFields.map {
        case (k @ (cr, fr), f) => k -> fieldValues.get(cr -> fr).fold(f) { data => f.copy(data = data) }
      } ++ thisFields.map { case (fr, fa) => ((thisRef -> fr) -> Field(fr.descriptor, fa, fieldValues(thisRef -> fr))) }

    def superClass: Class[_] = thisRef.superClassRef.loadClass

    lazy val thisFieldsSeq: Seq[(FieldRef, Data.Concrete)] = thisFields.keys.map { fr => fr -> fieldValues(thisRef, fr) }.toSeq
    lazy val superConstructor: Analyze.SetterConstructor =
      Analyze.findSetterConstructor(this, superClass, superFields) getOrElse {
        throw new TransformException(s"Usable constructor not found")
      }
    lazy val superConstructorArgs: Seq[Any] = superConstructor.toArguments(superFields)
    lazy val constructorArgs: Seq[(TypeRef.Public, Any)] =
      thisFieldsSeq
        .map { case (fr, data) => (fr.descriptor.typeRef -> data.concreteValue) } ++
        superConstructor.descriptor.args.zip(superConstructorArgs)

    lazy val constructorDescriptor = MethodDescriptor(TypeRef.Void, constructorArgs.map(_._1))
    lazy val constructorBody: MethodBody = {
      val thisFieldAssigns: Seq[(FieldRef, Int)] =
        thisFieldsSeq.zipWithIndex.map { case ((fr, f), i) => fr -> (i + 1) }
      import Bytecode._
      MethodBody(
        descriptor = constructorDescriptor,
        MethodAttribute.Public,
        codeFragment = new CodeFragment(
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
      klass.setModifiers(klass.getModifiers() | Modifier.PUBLIC)
      thisRef.interfaces.foreach { i =>
        klass.addInterface(classPool.get(i.getName))
      }
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
            minfo.setAccessFlags(body.attribute.toInt)
            klass.getClassFile.addMethod(minfo)
        }

      thisFields.foreach {
        case (ref, attr) =>
          val ctf = new CtField(ctClass(ref.descriptor.typeRef), ref.name, klass)
          ctf.setModifiers(attr.toInt)
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

      classLoader.registerClass(thisRef.name, klass.toBytecode)

      val concreteClass = classLoader.loadClass(thisRef.name)
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

  private[this] implicit class Upcast[A](val a: A) extends AnyVal {
    def upcast[B >: A]: B = a.asInstanceOf[B]
  }

  private def duplicate[A <: AnyRef, B >: A <: AnyRef](o: Instance[A], original: Original[_ <: A], superRef: ClassRef.Concrete, el: EventLogger): Duplicate[B] = {
    el.section("duplicate") { el =>
      el.log(s"from = ${o.thisRef}")
      el.log(s"new superclass = ${superRef}")

      // TODO: reject if dependent method is not moveable
      // TODO[BUG]: use same-runtime-package accessor class
      // TODO[BUG]: check resolved class reference is same as super class class loader's result

      val thisRef = superRef.extend(new AccessibleClassLoader(superRef.classLoader))
      val overridableVirtualMethods =
        o.virtualMethods.keySet
          .map { mr => o.resolveVirtualMethod(mr) -> mr }
          .filter { case (cr, _) => cr < ClassRef.Object }
          .filter { case k @ (cr, mr) => (cr < superRef) || !o.methods(k).isFinal }
          .filterNot { case k @ (cr, mr) => o.methods(k).isNative }
      el.logCMethods("overridable virtual methods", overridableVirtualMethods)

      val requiredMethods =
        o.extendMethods(overridableVirtualMethods)
          .filter { case (cr, _) => cr < ClassRef.Object }
          .filterNot { case k @ (cr, mr) => o.methods(k).isNative }
          .map { case k @ (cr, mr) => k -> o.dataflow(cr, mr) }
          .toMap
      el.logCMethods("required methods", requiredMethods.keys)

      val methodRenaming =
        requiredMethods.collect {
          case (k @ (cr, mr), df) if !overridableVirtualMethods.contains(k) =>
            (k -> mr.anotherUniqueName())
        }
      el.logCMethods("renamed methods", methodRenaming.keys)

      val requiredFields =
        requiredMethods.values.flatMap { df => df.usedFieldsOf(o) }
          .map { case (cr, fr) => o.resolveField(cr, fr) -> fr }
          .toSet
      el.logCFields("required fields", requiredFields)

      requiredFields foreach {
        case k @ (cr, fr) =>
          val f = o.fields(k)
          if (cr >= superRef && f.attribute.isPrivate && !f.attribute.isFinal)
            throw new TransformException(s"Required field is non-final private: $cr.$fr")
      }

      val fieldRenaming =
        requiredFields
          .filter { case k @ (cr, fr) => cr < superRef || o.fields(k).attribute.isPrivate }
          .map { case k @ (cr, fr) => k -> fr.anotherUniqueName() }
          .toMap
      el.logCFields("renamed fields", fieldRenaming.keys)

      val thisMethods =
        requiredMethods.map {
          case (k @ (cr, mr), df) =>
            val newMr = methodRenaming.get(k).getOrElse(mr)
            import Bytecode._
            newMr -> df.body.rewrite {
              case (label, bc @ invokevirtual(cr, mr)) if df.mustThis(label, bc.objectref) =>
                val vcr = o.resolveVirtualMethod(mr)
                methodRenaming.get(vcr -> mr).fold {
                  bc.rewriteClassRef(thisRef)
                } { newMr =>
                  bc.rewriteMethodRef(thisRef, newMr)
                }
              case (label, bc @ invokeinterface(cr, mr, _)) if df.mustThis(label, bc.objectref) =>
                val vcr = o.resolveVirtualMethod(mr)
                methodRenaming.get(vcr -> mr).fold {
                  bc.rewriteClassRef(thisRef)
                } { newMr =>
                  bc.rewriteMethodRef(thisRef, newMr)
                }
              case (label, bc @ invokespecial(cr, mr)) if df.mustThis(label, bc.objectref) =>
                // TODO: resolve special
                methodRenaming.get(cr -> mr).fold {
                  bc
                } { newMr =>
                  bc.rewriteMethodRef(thisRef, newMr)
                }
              case (label, bc: InstanceFieldAccess) if df.mustThis(label, bc.objectref) =>
                fieldRenaming.get(o.resolveField(bc.classRef, bc.fieldRef) -> bc.fieldRef).fold(bc) { newFr =>
                  bc.rewriteFieldRef(thisRef, newFr)
                }
            }.makeNonFinal
        }
      el.logMethods("thisMethods", thisMethods.keys)

      val thisFields =
        fieldRenaming.map {
          case (k @ (cr, fr), newFr) =>
            newFr -> o.fields(k).attribute
        }
      el.logFields("thisFields", thisFields.keys)

      val fieldValues =
        fieldRenaming.map {
          case (k @ (cr, fr), newFr) =>
            (thisRef.upcast[ClassRef] -> newFr) -> o.fields(k).data
        }
      el.logCFields("valued fields", fieldValues.keys)

      Duplicate[B](
        original,
        thisRef,
        thisMethods,
        thisFields,
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

