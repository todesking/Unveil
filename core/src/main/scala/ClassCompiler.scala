package com.todesking.unveil

import java.lang.reflect.Modifier

import scala.collection.JavaConversions._

class ClassCompiler(klass: Klass.Modified, fieldValues: Map[(ClassRef, FieldRef), Data.Concrete], el: EventLogger) {
  lazy val superClass: Class[_] = klass.ref.superClassRef.loadClass

  lazy val thisFieldsSeq: Seq[(FieldRef, Data.Concrete)] =
    klass.declaredFields.keys.map { fr => fr -> fieldValues(klass.ref, fr) }.toSeq

  lazy val superFields =
    klass.`super`.fieldAttributes.map { case (k, a) => k -> Field(k._2.descriptor, a, fieldValues(k)) }

  lazy val superConstructor: Analyze.SetterConstructor =
    Analyze.findSetterConstructor(klass.`super`, superFields) getOrElse {
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
              putfield(klass.ref, fr)
            )
        }.toSeq ++ Seq(vreturn())
    )
    )
  }

  def compile(): Klass.MaterializedNative = {
    import javassist.{ ClassPool, ClassClassPath, CtClass, CtMethod, CtField, CtConstructor, ByteArrayClassPath }
    import javassist.bytecode.{ Bytecode => JABytecode, MethodInfo }

    import Javassist.ctClass

    validate()

    val classLoader = klass.ref.classLoader

    val classPool = new ClassPool(null)
    Instance.findMaterializedClasses(classLoader).foreach {
      case (name, bytes) =>
        classPool.appendClassPath(new ByteArrayClassPath(name, bytes))
    }
    classPool.appendClassPath(new ClassClassPath(superClass))

    val ctBase = classPool.get(superClass.getName)

    val jClass = classPool.makeClass(klass.ref.name, ctBase)
    jClass.setModifiers(jClass.getModifiers() | Modifier.PUBLIC)
    klass.ref.interfaces.foreach { i =>
      jClass.addInterface(classPool.get(i.getName))
    }
    val constPool = jClass.getClassFile.getConstPool
    val ctObject = classPool.get("java.lang.Object")
    import Bytecode._
    klass.declaredMethods
      .foreach {
        case (ref, body) =>
          val codeAttribute = Javassist.compile(classPool, constPool, body.dataflow(klass))
          val minfo = new MethodInfo(constPool, ref.name, ref.descriptor.str)
          minfo.setCodeAttribute(codeAttribute)
          val sm = javassist.bytecode.stackmap.MapMaker.make(classPool, minfo)
          codeAttribute.setAttribute(sm)
          minfo.setAccessFlags(body.attribute.toInt)
          jClass.getClassFile.addMethod(minfo)
      }

    klass.declaredFields.foreach {
      case (ref, attr) =>
        val ctf = new CtField(ctClass(ref.descriptor.typeRef), ref.name, jClass)
        ctf.setModifiers(attr.toInt)
        jClass.addField(ctf)
    }

    val ctor = new CtConstructor(constructorArgs.map(_._1).map(ctClass).toArray, jClass)
    jClass.addConstructor(ctor)

    val ctorMethodInfo =
      jClass
        .getClassFile
        .getMethods
        .map(_.asInstanceOf[MethodInfo])
        .find(_.getName == "<init>")
        .get

    val ctorCA = Javassist.compile(classPool, constPool, constructorBody.dataflow(klass))
    ctorMethodInfo.setCodeAttribute(ctorCA)
    val sm = javassist.bytecode.stackmap.MapMaker.make(classPool, ctorMethodInfo)
    ctorCA.setAttribute(sm)

    classLoader.registerClass(klass.ref.name, jClass.toBytecode)
    val concreteClass = classLoader.loadClass(klass.ref.name)

    val bytes = jClass.toBytecode
    Instance.registerMaterialized(classLoader, jClass.getName, bytes)

    new Klass.MaterializedNative(concreteClass, constructorArgs)
  }

  private[this] def validate(): Unit = {
    def fail(msg: String) =
      throw new IllegalStateException(msg)

    klass.requireWholeInstanceField(fieldValues.keySet)

    if ((klass.ref.superClassRef.loadClass.getModifiers & Modifier.FINAL) == Modifier.FINAL)
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

object ClassCompiler {
  def compile(klass: Klass.Modified, fieldValues: Map[(ClassRef, FieldRef), Data.Concrete], el: EventLogger): Klass.MaterializedNative =
    new ClassCompiler(klass, fieldValues, el).compile()
}

