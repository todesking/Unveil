package com.todesking.unveil

import javassist.{ ClassPool, CtClass, CtBehavior }
import javassist.bytecode.{ CodeAttribute, ConstPool, Bytecode => JABytecode, MethodInfo }
import java.lang.reflect.{ Method => JMethod, Constructor }

import scala.collection.mutable

object Javassist {
  def ctClass(tr: TypeRef): CtClass = {
    tr match {
      case TypeRef.Int => CtClass.intType
      case TypeRef.Reference(ClassRef.Concrete(name, cl)) =>
        val pool = buildPool(cl)
        pool.get(name)
      case unk => throw new NotImplementedError(s"${unk}")
    }
  }

  def compile(classPool: ClassPool, constPool: ConstPool, df: DataFlow): CodeAttribute = {
    val body = df.body
    val ctObject = classPool.get("java.lang.Object")
    val out = new JABytecode(constPool, 0, 0)
    val jumps = mutable.HashMap.empty[Int, (Int, JumpTarget)] // jump operand address -> (insn addr -> target)
    val addrs = mutable.HashMap.empty[Bytecode.Label, Int]
    import Bytecode._
    body.bytecode foreach { bc =>
      addrs(bc.label) = out.getSize
      bc match {
        case nop() =>
          out.add(0x00)
        case aconst_null() =>
          out.addConstZero(ctObject)
        case vreturn() =>
          out.addReturn(null)
        case ireturn() =>
          out.addReturn(CtClass.intType)
        case lreturn() =>
          out.addReturn(CtClass.longType)
        case areturn() =>
          out.add(0xB0)
        case dreturn() =>
          out.add(0xAF)
        case iload(n) =>
          out.addIload(n)
        case aload(n) =>
          out.addAload(n)
        case istore(n) =>
          out.addIstore(n)
        case astore(n) =>
          out.addAstore(n)
        case iconst(c) =>
          out.addIconst(c)
        case lconst(c) =>
          out.addLconst(c)
        case goto(target) =>
          out.add(0xA7)
          jumps(out.getSize) = (out.getSize - 1) -> target
          out.add(0x00, 0x03)
        case dup() =>
          out.add(0x59)
        case pop() =>
          out.add(0x57)
        case pop2() =>
          out.add(0x58)
        case iadd() =>
          out.add(0x60)
        case isub() =>
          out.add(0x64)
        case if_acmpne(target) =>
          out.add(0xA6)
          jumps(out.getSize) = (out.getSize - 1) -> target
          out.add(0x00, 0x03)
        case invokevirtual(classRef, methodRef) =>
          // TODO: check resolved class
          out.addInvokevirtual(classRef.binaryName, methodRef.name, methodRef.descriptor.str)
        case invokespecial(classRef, methodRef) =>
          // TODO: check resolved class
          out.addInvokespecial(classRef.binaryName, methodRef.name, methodRef.descriptor.str)
        case invokestatic(classRef, methodRef) =>
          out.addInvokestatic(classRef.binaryName, methodRef.name, methodRef.descriptor.str)
        case if_icmpge(target) =>
          out.add(0xA2)
          jumps(out.getSize) = (out.getSize - 1) -> target
          out.add(0x00, 0x03)
        case if_icmple(target) =>
          out.add(0xA4)
          jumps(out.getSize) = (out.getSize - 1) -> target
          out.add(0x00, 0x03)
        case ifnonnull(target) =>
          out.add(0xC7)
          jumps(out.getSize) = (out.getSize - 1) -> target
          out.add(0x00, 0x03)
        case athrow() =>
          out.add(0xBF)
        case getfield(classRef, fieldRef) =>
          out.addGetfield(classRef.binaryName, fieldRef.name, fieldRef.descriptor.str)
        case getstatic(classRef, fieldRef) =>
          out.addGetstatic(classRef.binaryName, fieldRef.name, fieldRef.descriptor.str)
        case putfield(classRef, fieldRef) =>
          out.addPutfield(classRef.binaryName, fieldRef.name, fieldRef.descriptor.str)
      }
    }
    jumps foreach {
      case (dataIndex, (index, target)) =>
        val label = body.jumpTargets(target)
        val targetIndex = addrs(label)
        out.write16bit(dataIndex, targetIndex - index)
    }
    out.setMaxLocals(df.maxLocals)
    out.setMaxStack(df.maxStackDepth)
    out.toCodeAttribute
  }

  def decompile(m: JMethod): Option[MethodBody] = {
    require(m != null)

    val jClass = m.getDeclaringClass
    val classPool = buildPool(jClass)

    val ctClass = classPool.get(jClass.getName)
    val mRef = MethodRef.from(m)

    val ctMethod = ctClass.getMethod(mRef.name, mRef.descriptor.str)

    decompile0(jClass, mRef, ctMethod)
  }

  def decompile(m: Constructor[_]): Option[MethodBody] = {
    val classPool = buildPool(m.getDeclaringClass)
    val jClass = m.getDeclaringClass
    val ctClass = classPool.get(jClass.getName)
    val mRef = MethodRef.from(m)
    val ctMethod = ctClass.getConstructor(mRef.descriptor.str)
    decompile0(jClass, mRef, ctMethod)
  }

  private[this] def buildPool(jClass: Class[_]): ClassPool = {
    import javassist.{ ClassClassPath, ByteArrayClassPath }

    val classPool = new ClassPool(null)
    Instance.findMaterializedClasses(jClass.getClassLoader).foreach {
      case (name, bytes) =>
        classPool.appendClassPath(new ByteArrayClassPath(name, bytes))
    }
    classPool.appendClassPath(new ClassClassPath(jClass))
    classPool
  }

  private[this] def buildPool(cl: ClassLoader): ClassPool = {
    import javassist.{ LoaderClassPath, ByteArrayClassPath }

    val classPool = new ClassPool(null)
    Instance.findMaterializedClasses(cl).foreach {
      case (name, bytes) =>
        classPool.appendClassPath(new ByteArrayClassPath(name, bytes))
    }
    classPool.appendClassPath(new LoaderClassPath(if (cl == null) ClassLoader.getSystemClassLoader else cl))
    classPool
  }

  private[this] def decompile0(jClass: Class[_], mRef: MethodRef, ctMethod: CtBehavior): Option[MethodBody] = {
    if (ctMethod.getMethodInfo2.getCodeAttribute == null) {
      None
    } else {
      val isStatic = (ctMethod.getMethodInfo2.getAccessFlags & 0x08) == 0x08

      val codeAttribute = ctMethod.getMethodInfo2.getCodeAttribute
      val it = codeAttribute.iterator
      val cpool = ctMethod.getDeclaringClass.getClassFile.getConstPool
      val bcs = mutable.ArrayBuffer.empty[Bytecode]
      val jumpTargets = mutable.HashMap.empty[JumpTarget, Bytecode.Label]
      val addr2jt = JumpTarget.assigner[Int]()

      def onInstruction(index: Int, bc: Bytecode): Unit = {
        bcs += bc
        jumpTargets(addr2jt(index)) = bc.label
      }

      while (it.hasNext) {
        val index = it.next()
        import Bytecode._
        it.byteAt(index) match {
          case 0x00 => // nop
            onInstruction(index, nop())
          case 0x01 => // aconst_null
            onInstruction(index, aconst_null())
          // TODO
          case 0x03 => // iconst_0
            onInstruction(index, iconst(0))
          case 0x04 => // iconst_1
            onInstruction(index, iconst(1))
          case 0x05 => // iconst_2
            onInstruction(index, iconst(2))
          case 0x06 => // iconst_3
            onInstruction(index, iconst(3))
          case 0x07 => // iconst_4
            onInstruction(index, iconst(4))
          case 0x08 => // iconst_5
            onInstruction(index, iconst(5))
          case 0x09 => // lconst_0
            onInstruction(index, lconst(0))
          // TODO
          case 0x10 => // bipush
            onInstruction(index, iconst(it.signedByteAt(index + 1)))
          case 0x11 => // sipush
            onInstruction(index, iconst(it.s16bitAt(index + 1)))
          // TODO
          case 0x1B => // iload_1
            onInstruction(index, iload(1))
          // TODO
          case 0x2A => // aload_0
            onInstruction(index, aload(0))
          case 0x2B => // aload_1
            onInstruction(index, aload(1))
          case 0x2C => // aload_2
            onInstruction(index, aload(2))
          case 0x2D => // aload_3
            onInstruction(index, aload(3))
          // TODO
          case 0x3C => // istore_1
            onInstruction(index, istore(1))
          // TODO
          case 0xA2 => // if_icmpge
            onInstruction(index, if_icmpge(addr2jt(index + it.s16bitAt(index + 1))))
          case 0xA4 => // if_icmple
            onInstruction(
              index,
              if_icmple(addr2jt(index + it.s16bitAt(index + 1)))
            )
          // TODO
          case 0xA6 => // if_acmpne
            onInstruction(index, if_acmpne(addr2jt(index + it.s16bitAt(index + 1))))
          case 0xA7 => // goto
            onInstruction(index, goto(addr2jt(index + it.s16bitAt(index + 1))))
          // TODO
          case 0xAC => // ireturn
            onInstruction(index, ireturn())
          // TODO
          case 0xAD => // lreturn
            onInstruction(index, lreturn())
          // TODO
          case 0xAF => // dreturn
            onInstruction(index, dreturn())
          case 0x60 => // iadd
            onInstruction(index, iadd())
          case 0x64 => // isub
            onInstruction(index, isub())
          // TODO
          case 0xB0 => // areturn
            onInstruction(index, areturn())
          case 0xB1 => // return
            onInstruction(index, vreturn())
          // TODO
          case 0xB2 => // getstatic
            val constIndex = it.u16bitAt(index + 1)
            val className = cpool.getFieldrefClassName(constIndex)
            val classRef = ClassRef.of(jClass.getClassLoader.loadClass(className))
            val fieldName = cpool.getFieldrefName(constIndex)
            val fieldDescriptor = FieldDescriptor.parse(cpool.getFieldrefType(constIndex), jClass.getClassLoader)
            val fieldRef = FieldRef(fieldName, fieldDescriptor)
            onInstruction(index, getstatic(classRef, fieldRef))
          case 0xB4 => // getfield
            val constIndex = it.u16bitAt(index + 1)
            val className = cpool.getFieldrefClassName(constIndex)
            val classRef = ClassRef.of(jClass.getClassLoader.loadClass(className))
            val fieldName = cpool.getFieldrefName(constIndex)
            val fieldDescriptor = FieldDescriptor.parse(cpool.getFieldrefType(constIndex), jClass.getClassLoader)
            val fieldRef = FieldRef(fieldName, fieldDescriptor)
            onInstruction(index, getfield(classRef, fieldRef))
          case 0xB5 => // putfield
            val constIndex = it.u16bitAt(index + 1)
            val className = cpool.getFieldrefClassName(constIndex)
            val classRef = ClassRef.of(jClass.getClassLoader.loadClass(className))
            val fieldName = cpool.getFieldrefName(constIndex)
            val fieldDescriptor = FieldDescriptor.parse(cpool.getFieldrefType(constIndex), jClass.getClassLoader)
            val fieldRef = FieldRef(fieldName, fieldDescriptor)
            onInstruction(index, putfield(classRef, fieldRef))
          case 0xB6 => // invokevirtual
            val constIndex = it.u16bitAt(index + 1)
            val className = cpool.getMethodrefClassName(constIndex)
            val methodName = cpool.getMethodrefName(constIndex)
            val methodType = cpool.getMethodrefType(constIndex)
            val classRef = ClassRef.of(className, jClass.getClassLoader)
            onInstruction(
              index,
              invokevirtual(
                classRef,
                MethodRef(methodName, MethodDescriptor.parse(methodType, jClass.getClassLoader))
              )
            )
          case 0xB7 => // invokespecial
            val constIndex = it.u16bitAt(index + 1)
            val className = cpool.getMethodrefClassName(constIndex)
            val methodName = cpool.getMethodrefName(constIndex)
            val methodType = cpool.getMethodrefType(constIndex)
            val classRef = ClassRef.of(className, jClass.getClassLoader)
            onInstruction(
              index,
              invokespecial(
                classRef,
                MethodRef(methodName, MethodDescriptor.parse(methodType, jClass.getClassLoader))
              )
            )
          case 0xB8 => // invokestatic
            val constIndex = it.u16bitAt(index + 1)
            val className = cpool.getMethodrefClassName(constIndex)
            val methodName = cpool.getMethodrefName(constIndex)
            val methodType = cpool.getMethodrefType(constIndex)
            val classRef = ClassRef.of(className, jClass.getClassLoader)
            onInstruction(
              index,
              invokestatic(
                classRef,
                MethodRef(methodName, MethodDescriptor.parse(methodType, jClass.getClassLoader))
              )
            )
          case 0xBF => // athrow
            onInstruction(index, athrow())
          case 0xC7 => // ifnonnull
            onInstruction(index, ifnonnull(addr2jt(index + it.s16bitAt(index + 1))))
          case unk =>
            throw new UnsupportedOpcodeException(ClassRef.of(jClass), mRef, unk)
        }
      }
      Some(MethodBody(
        mRef.descriptor,
        MethodAttribute.from(ctMethod.getModifiers),
        bcs.toSeq,
        jumpTargets.toMap
      ))
    }
  }

  // TODO: Make javassist getItem to public
  def printConstPool(cfile: javassist.bytecode.ClassFile): Unit = {
    val cop = cfile.getConstPool
    val gi = cop.getClass.getDeclaredMethods.find(_.getName == "getItem").get
    gi.setAccessible(true)
    (1 until cop.getSize) foreach { i =>
      val a = gi.invoke(cop, i.asInstanceOf[java.lang.Integer])
      val x = a.getClass.getMethods.find(_.getName == "print").get
      x.setAccessible(true)
      val pw = new java.io.PrintWriter(System.out)
      println(s"${i} -> ${a.getClass}")
      print("  ")
      x.invoke(a, pw)
      pw.flush()
    }
  }
}
