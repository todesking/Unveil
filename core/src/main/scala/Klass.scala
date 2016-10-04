package com.todesking.unveil

sealed abstract class Klass {
  def name: String = ref.name
  def ref: ClassRef
  def methodBody(cr: ClassRef, mr: MethodRef): MethodBody
  def instanceMethods: Map[(ClassRef, MethodRef), MethodAttribute]
  def instanceFieldAttributes: Map[(ClassRef, FieldRef), FieldAttribute]
  def virtualMethods: Set[MethodRef] =
    instanceMethods.filter(_._2.isVirtual).map { case ((cr, mr), a) => mr }.toSet
  def hasVirtualMethod(ref: MethodRef): Boolean =
    instanceMethods.exists { case ((c, m), a) => m == ref && a.isVirtual }
  def hasVirtualMethod(mr: String): Boolean =
    hasVirtualMethod(MethodRef.parse(mr, ref.classLoader))
  def resolveVirtualMethod(mr: MethodRef): ClassRef
  def superKlass: Option[Klass]
  def hasInstanceField(cr: ClassRef, fr: FieldRef): Boolean =
    instanceFieldAttributes.contains(cr -> fr)
  def dataflow(cr: ClassRef, mr: MethodRef): DataFlow =
    new DataFlow(methodBody(cr, mr), this, Map(), None)
  def pretty: String = Pretty.format_Klass(this)

  // TODO: interface field???
  def resolveInstanceField(cr: ClassRef, fr: FieldRef): ClassRef =
    if (hasInstanceField(cr, fr)) cr
    else Reflect.superClassOf(cr).map { sc => resolveInstanceField(sc, fr) } getOrElse {
      throw new IllegalArgumentException(s"Instance field resolution failed: $cr.$fr")
    }

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
        .flatMap { case (cr, mr) => dataflow(cr, mr).usedMethodsOf(DataSource.This, this) }
    } while (ext.size > ms.size)
    ext
  }

  def requireWholeInstanceField(fvs: Set[(ClassRef, FieldRef)]): Unit = {
    val nonStaticFields = instanceFieldAttributes.keySet
    if((nonStaticFields -- fvs).nonEmpty) {
      throw new IllegalArgumentException(s"Field value missing: ${nonStaticFields -- fvs}")
    } else if((fvs -- nonStaticFields).nonEmpty) {
      throw new IllegalArgumentException(s"Unknown field value: ${fvs -- nonStaticFields}")
    }
  }

  // return: field -> new field name
  def duplicate(superRef: ClassRef.Concrete, el: EventLogger): (Klass.Modified, Map[(ClassRef, FieldRef), FieldRef]) =
    el.section("Klass.duplicate") { el =>
      el.log(s"from = ${ref}")
      el.log(s"new superclass = ${superRef}")
      require(superRef >= ref)

      // TODO: reject if dependent method is not moveable
      // TODO[BUG]: use same-runtime-package accessor class
      // TODO[BUG]: check resolved class reference is same as super class class loader's result

      val thisRef = superRef.extend(new AccessibleClassLoader(superRef.classLoader))
      val overridableVirtualMethods =
        this.virtualMethods
          .map { mr => this.resolveVirtualMethod(mr) -> mr }
          .filter { case (cr, _) => cr < ClassRef.Object }
          .filter { case k @ (cr, mr) => (cr < superRef) || !this.instanceMethods(k).isFinal }
          .filterNot { case k @ (cr, mr) => this.instanceMethods(k).isNative }
      el.logCMethods("overridable virtual instanceMethods", overridableVirtualMethods)

      val requiredMethods =
        this.extendMethods(overridableVirtualMethods)
          .filter { case (cr, _) => cr < ClassRef.Object }
          .filterNot { case k @ (cr, mr) => this.instanceMethods(k).isNative }
          .map { case k @ (cr, mr) => k -> this.dataflow(cr, mr) }
          .toMap
      el.logCMethods("required instanceMethods", requiredMethods.keys)

      val methodRenaming =
        requiredMethods.collect {
          case (k @ (cr, mr), df) if !overridableVirtualMethods.contains(k) =>
            (k -> mr.anotherUniqueName())
        }
      el.logCMethods("renamed instanceMethods", methodRenaming.keys)

      val requiredFields =
        requiredMethods.values.flatMap { df => df.usedFieldsOf(DataSource.This, this) }
          .map { case (cr, fr) => this.resolveInstanceField(cr, fr) -> fr }
          .toSet
      el.logCFields("required fields", requiredFields)

      requiredFields foreach {
        case k @ (cr, fr) =>
          val fa = this.instanceFieldAttributes(k)
          if (cr >= superRef && fa.isPrivate && !fa.isFinal)
            throw new TransformException(s"Required field is non-final private: $cr.$fr")
      }

      val fieldRenaming =
        requiredFields
          .filter { case k @ (cr, fr) => cr < superRef || this.instanceFieldAttributes(k).isPrivate }
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
                val vcr = this.resolveVirtualMethod(mr)
                methodRenaming.get(vcr -> mr).fold {
                  bc.rewriteClassRef(thisRef)
                } { newMr =>
                  bc.rewriteMethodRef(thisRef, newMr)
                }
              case (label, bc @ invokeinterface(cr, mr, _)) if df.mustThis(label, bc.objectref) =>
                val vcr = this.resolveVirtualMethod(mr)
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
                fieldRenaming.get(this.resolveInstanceField(bc.classRef, bc.fieldRef) -> bc.fieldRef).fold(bc) { newFr =>
                  bc.rewriteFieldRef(thisRef, newFr)
                }
            }.makeNonFinal
        }
      el.logMethods("thisMethods", thisMethods.keys)

      val thisFields =
        fieldRenaming.map {
          case (k @ (cr, fr), newFr) =>
            newFr -> this.instanceFieldAttributes(k)
        }
      el.logFields("thisFields", thisFields.keys)

      new Klass.Modified(superRef.loadKlass, thisRef, thisMethods, thisFields) -> fieldRenaming
    }
}
object Klass {
  def from(j: Class[_]): Native =
    new Native(j)

  class Native(val javaClass: Class[_]) extends Klass with Equality.Delegate {
    override def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[Native]
    override val equalityObject = javaClass

    override lazy val superKlass =
      if (javaClass.getSuperclass == null) None
      else Some(Klass.from(javaClass.getSuperclass))

    override def ref: ClassRef.Concrete = ClassRef.of(javaClass)

    override def resolveVirtualMethod(mr: MethodRef): ClassRef =
      Reflect.resolveVirtualMethod(javaClass, mr)

    override def methodBody(cr: ClassRef, mr: MethodRef) =
      if (mr.isInit) MethodBody.parse(allJConstructors(cr -> mr))
      else MethodBody.parse(allJMethods(cr -> mr))

    override lazy val instanceMethods: Map[(ClassRef, MethodRef), MethodAttribute] =
      allJMethods.map { case (k, m) => k -> MethodAttribute.from(m) }.filterNot(_._2.isStatic)

    override def instanceFieldAttributes =
      allJFields.mapValues(FieldAttribute.from(_)).filter(!_._2.isStatic)

    def readField(obj: AnyRef, cr: ClassRef, fr: FieldRef): Field =
      Field.from(allJFields(cr -> fr), obj)

    private[this] lazy val virtualJMethods = Reflect.virtualJMethods(javaClass)
    private[this] lazy val allJMethods = Reflect.allJMethods(javaClass)
    private[this] lazy val allJFields = Reflect.allJFields(javaClass)
    private[this] lazy val allJConstructors = Reflect.allJConstructors(javaClass)

  }

  class MaterializedNative(
      jc: Class[_],
      val constructorArgs: Seq[(TypeRef.Public, Any)]
  ) extends Native(jc) {
    // TODO: add type parameter
    def newInstance[A <: AnyRef](): Instance.Original[A] = {
      val value =
        try {
          javaClass
            .getDeclaredConstructor(constructorArgs.map(_._1.javaClass).toArray: _*)
            .newInstance(constructorArgs.map(_._2.asInstanceOf[Object]).toArray: _*)
        } catch {
          case e: LinkageError => throw new InvalidClassException(this, e)
        }
      Instance.of(value.asInstanceOf[A])
    }
  }

  class Modified(
      val `super`: Klass.Native,
      override val ref: ClassRef.Extend,
      val declaredMethods: Map[MethodRef, MethodBody],
      val declaredFields: Map[FieldRef, FieldAttribute]
  ) extends Klass with Equality.Reference {
    require(!declaredFields.exists(_._2.isStatic))

    override def instanceFieldAttributes: Map[(ClassRef, FieldRef), FieldAttribute] =
      `super`.instanceFieldAttributes ++ declaredFields.map { case (fr, fa) => (ref, fr) -> fa }

    override def superKlass: Option[Klass] = Some(`super`)

    // TODO: support default interface method
    override def resolveVirtualMethod(mr: MethodRef): ClassRef =
      declaredMethods.get(mr).map { body =>
        if (body.attribute.isVirtual) ref
        else throw new IllegalArgumentException(s"Not virtual: ${mr} ${body.attribute}")
      } getOrElse {
        `super`.resolveVirtualMethod(mr)
      }

    override def methodBody(cr: ClassRef, mr: MethodRef) =
      if (cr == ref) declaredMethods(mr)
      else if (ref < cr) `super`.methodBody(cr, mr)
      else throw new IllegalArgumentException(s"Method not found: ${cr.pretty}.${mr.str}")

    override lazy val instanceMethods =
      `super`.instanceMethods ++ declaredMethods.map {
        case (k, v) => (ref -> k) -> v.attribute
      }

    def addMethod(mr: MethodRef, body: MethodBody): Modified = {
      require(mr.descriptor == body.descriptor)
      require(!body.attribute.isStatic)
      new Modified(`super`, ref, declaredMethods + (mr -> body), declaredFields)
    }

    def addField(fr: FieldRef, attribute: FieldAttribute): Modified = {
      require(!attribute.isStatic)
      new Modified(`super`, ref, declaredMethods, declaredFields + (fr -> attribute))
    }

    def changeRef(newRef: ClassRef.Extend): Modified = {
      require(newRef.superClassRef == ref.superClassRef)
      new Modified(
        `super`,
        newRef,
        declaredMethods
          .map { case (mr, body) => mr -> body.rewriteClassRef(ref, newRef) },
        declaredFields
      )
    }

    def materialize(fieldValues: Map[(ClassRef, FieldRef), Data.Concrete], el: EventLogger): MaterializedNative =
      ClassCompiler.compile(this, fieldValues, el)
  }
}
