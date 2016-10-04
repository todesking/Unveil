package com.todesking.unveil

import com.todesking.scalapp.syntax._

import org.scalatest.{ FunSpec, Matchers, Failed }

class Spec extends FunSpec with Matchers {
  def dotBody(filename: String, self: Instance[_ <: AnyRef], b: MethodBody): Unit = {
    import java.nio.file._
    // TODO: DataFlow.toDot
    // Files.write(Paths.get(filename), b.dataflow(self).toDot.getBytes("UTF-8"))
  }

  def withThe[A](i: Instance[_])(f: => A): A = {
    try { f } catch { case e: Throwable => println("==== TEST FAILED\n" + i.pretty); throw e }
  }

  val el = Transformer.newEventLogger()

  override def withFixture(test: NoArgTest) = {
    def printEvents(): Unit = {
      println("=== EVENT LOG:")
      println(el.pretty)
    }
    val result = try {
      super.withFixture(test)
    } catch {
      case e: Throwable =>
        println("=== TEST FAILED")
        printEvents()
        throw e
    }
    result match {
      case Failed(t: UnveilException.HasMethodBody) =>
        println(s"=== FAILED($t)")
        println(t)
        println(t.methodBody.pretty)
        printEvents()
      case o @ Failed(t: InvalidClassException) =>
        println("=== INVALID CLASS")
        println(t.klass.pretty)
        printEvents()
      case Failed(t: UnveilBugException) =>
        println(t.detail)
        printEvents()
      case Failed(t) =>
        println(s"=== TEST FAILED($t)")
        printEvents()
      case _ =>
    }
    el.clear()
    result
  }

  describe("opt") {
    val defaultCL = ClassLoader.getSystemClassLoader
    it("duplicate") {
      class Const {
        def intMethod(): Int = 1
        def longMethod(): Long = 0L
      }
      val obj = new Const
      val i = Instance.of(obj)
      i.klass.hasVirtualMethod("intMethod()I") should be(true)

      val intMethod = MethodRef.parse("intMethod()I", defaultCL)
      val longMethod = MethodRef.parse("longMethod()J", defaultCL)
      val ri = i.duplicate[Const](el)
      withThe(ri) {
        ri.materialized.value.intMethod() should be(1)
        ri.materialized.value.longMethod() should be(0L)
      }
    }
    it("invokeVirtual with no arguments") {
      class InvokeVirtual0 {
        def foo(): Int = bar()
        def bar(): Int = 1
      }
      val d = new InvokeVirtual0
      d.foo() should be(1)

      val i = Instance.of(d)
      val foo = MethodRef.parse("foo()I", defaultCL)

      val ri = i.duplicate(el).materialized

      ri.value.foo() should be(1)
    }
    it("invokeVirtual1") {
      class InvokeVirtual1 {
        def foo(): Int = bar(1)
        def bar(n: Int): Int = n
      }
      val d = new InvokeVirtual1
      d.foo() should be(1)

      val i = Instance.of(d)
      val foo = MethodRef.parse("foo()I", defaultCL)

      val ri = i.duplicate(el).materialized

      ri.value.foo() should be(1)
    }
    it("if") {
      class If {
        def foo(a: Int): Int =
          if (a > 0) 100
          else if (a > -10) -10
          else -100
      }
      val d = new If
      d.foo(1) should be(100)
      d.foo(-1) should be(-10)
      d.foo(-11) should be(-100)

      val i = Instance.of(d)
      val foo = MethodRef.parse("foo(I)I", defaultCL)

      val ri = i.duplicate(el).materialized
      ri.value.foo(1) should be(100)
      ri.value.foo(-1) should be(-10)
      ri.value.foo(-11) should be(-100)
    }
    it("other method") {
      object OtherMethod {
        abstract class A {
          def foo(): Int
          def bar(): Int = 10
        }
        class B extends A {
          override def foo() = baz()
          override def bar() = 99
          def baz() = bar()
        }
      }
      val obj = new OtherMethod.B
      obj.foo() should be(99)
      val i = Instance.of(obj)
      val foo = MethodRef.parse("foo()I", defaultCL)
      val ri = i.duplicate(el).materialized
      ri.value.foo() should be(99)
    }
    it("real upcast") {
      abstract class A {
        def foo(): Int
        def bar(): Int = 10
      }
      final class B extends A {
        override def foo() = baz()
        override def bar() = 99
        def baz() = bar()
      }
      val obj = new B
      obj.foo() should be(99)
      val i = Instance.of[A](obj)
      val foo = MethodRef.parse("foo()I", defaultCL)
      val ri = i.duplicate[A](el).materialized
      dotBody("real_upcast.dot", ri, ri.methodBody(foo))
      classOf[A].isAssignableFrom(ri.value.getClass) should be(true)
      classOf[B].isAssignableFrom(ri.value.getClass) should be(false)
      ri.value.foo() should be(99)
    }
    it("simple dataflow compile") {
      class A {
        def foo(): Int = if (bar > 20) 1 else 2
        def bar(): Int = 10
      }

      val i = Instance.of(new A)
      i.value.foo() should be(2)

      val foo = MethodRef.parse("foo()I", defaultCL)

      val ri = i.duplicate(el).materialized

      dotBody("s.dot", ri, ri.methodBody(foo))

      ri.value.foo() should be(2)
    }
    it("primitive field") {
      class A {
        val foo = 10
      }
      val foo = MethodRef.parse("foo()I", defaultCL)

      val i = Instance.of(new A)
      i.value.foo should be(10)

      val ri = i.duplicate(el).materialized
      ri.value.foo should be(10)
    }
    it("field duplicate") {
      abstract class Base {
        def foo: Int
      }
      class A extends Base {
        val x = 1000
        override def foo = x
      }
      class B extends A {
      }
      val i = Instance.of(new B)
      i.value.foo should be(1000)

      val ri = i.duplicate[Base](el).materialized
      ri.value.foo should be(1000)
    }
    it("dupdup") {
      abstract class Base {
        def foo: Int
      }
      class A extends Base {
        val x = 1000
        override def foo = x
      }
      class B extends A {
      }
      val i = Instance.of(new B)
      i.value.foo should be(1000)

      val ri = i.duplicate[Base](el).duplicate[Base](el).materialized
      ri.value.foo should be(1000)
    }
    it("dup and accessor") {
      abstract class Base { def foo: Int }
      class A extends Base { override val foo = 1 }
      val dup = Instance.of(new A).duplicate[Base](el)
      dup.materialized.value.foo should be(1)
    }
    describe("field fusion") {
      it("when empty") {
        class A {
          def foo(): Int = 1
        }
        val expected = 1

        val i = Instance.of(new A)
        i.value.foo() should be(expected)

        val fi = Transformer.fieldFusion.apply(i, el).get
        fi.materialized.value.foo should be(expected)
      }
      it("simple") {
        class A {
          def foo(): Int = 1
        }
        class B {
          def bar(): Int = 10
        }
        val expected = 10

        val foo = MethodRef.parse("foo()I", defaultCL)
        val bar = MethodRef.parse("bar()I", defaultCL)
        val fieldB = FieldRef("b", FieldDescriptor(TypeRef.Reference(ClassRef.of(classOf[B]))))

        val b = Instance.of(new B)
        val i0 = Instance.of(new A)
          .duplicate1(el)
          .addField(fieldB, Field(fieldB.descriptor, FieldAttribute.Final, Data.ConcreteReference(b)))
        val i =
          i0.addMethod(
            foo,
            MethodBody(
              foo.descriptor,
              MethodAttribute.Public,
              CodeFragment.bytecode(
                Bytecode.aload(0),
                Bytecode.getfield(i0.thisRef, fieldB),
                Bytecode.invokevirtual(b.thisRef, bar),
                Bytecode.ireturn()
              )
            )
          )
        withThe(i) {
          i.materialized.value.foo() should be(expected)
        }

        val fi = withThe(i) {
          Transformer.fieldFusion(i, el).get
        }
        withThe(fi) {
          fi.materialized.value.foo() should be(expected)
        }
      }
      it("nested") {
        class A(b: B) {
          def bbb = b
          def foo(): Int = b.bar() + 1000
        }
        class B(c: C) {
          def bar(): Int = c.baz() + 1
        }
        class C {
          def baz(): Int = 999
        }
        val expected = 2000
        val foo = MethodRef.parse("foo()I", defaultCL)
        val bar = MethodRef.parse("bar()I", defaultCL)

        val c = new C
        val b = new B(c)
        val a = new A(b)
        a.foo() should be(expected)

        val i = Instance.of(a)

        val fused = Transformer.fieldFusion(i, el).get
        withThe(fused) {
          fused.dataflow(foo).usedFieldsOf(fused) should be('empty)
          fused.usedMethodsOf(Instance.of(c)) should be('empty)
          fused.materialized.value.foo() should be(expected)
        }
      }

      it("Function1") {
        val f1 = { n: Int => n + 1 }
        val f2 = { n: Int => n * 2 }
        val f3 = { n: Int => n + 1 }
        val f = f1 andThen f2 andThen f3
        val i = Instance.of(f)
        val n = 1
        val expected = 5
        i.materialized.value.apply(n) should be(expected)

        val dup = i.duplicate[Int => Int](el)
        withThe(dup) {
          dup.materialized.value.apply(n) should be(expected)
        }

        val ti = Transformer.fieldFusion(i.duplicate[Int => Int](el), el).get
        withThe(ti) {
          ti.materialized.value.apply(n) should be(expected)
        }
      }
    }

    describe("method inlining") {
      it("no control") {
        class A {
          def foo(): Int = bar() + baz()
          def bar(): Int = 10
          private[this] def baz(): Int = 90
        }
        val expected = 100
        val foo = MethodRef.parse("foo()I", defaultCL)

        val i = Instance.of(new A)
        i.value.foo() should be(expected)

        val ri = Transformer.methodInlining(i, el).get
        withThe(ri) {
          ri.materialized.value.foo() should be(expected)
          ri.dataflow(foo).usedMethodsOf(ri) should be('empty)
        }
      }
      it("control") {
        class A {
          def foo(): Int = bar(10) + baz(1) + baz(0)
          def bar(n: Int): Int = if (n > 0) 10 else 20
          private[this] def baz(n: Int): Int = if (n < 0) n + 1 else if (n > 0) n - 1 else 10
        }
        val expected = 10 + 0 + 10
        val foo = MethodRef.parse("foo()I", defaultCL)

        val i = Instance.of(new A)
        i.value.foo() should be(expected)

        val ri = Transformer.methodInlining(i, el).get
        withThe(ri) {
          ri.materialized.value.foo() should be(expected)
          ri.dataflow(foo).usedMethodsOf(ri) should be('empty)
        }
      }
      it("Function1") {
        pending
      }
    }

    describe("new instance") {
      it("handle new insn") {
        class A(val value: Int) {
          def foo(): A = new A(2)
        }
        val x = Instance.of(new A(1)).duplicate[A](el)
        x.materialized.value.foo.value should be(2)
      }
      it("inline local instance") {
        class A(val value: Int) {
          def foo(): Int = new A(2).value + value
        }
        val i = Instance.of(new A(1))
        val ri = Transformer.localInstanceInlining(i, el).get
        withThe(ri) {
          ri.materialized.value.foo() should be(3)
          ri.methodBody(MethodRef.parse("foo()I", defaultCL))
            .bytecode.collect { case x@(l, Bytecode.new_(_)) => x } should be('empty)
        }
      }
    }

    it("double values") {
      pending
    }

    // TODO: Exception handler rejection test
    // TODO: accessor inlining
    // TODO: accept new instance as constant in SetterConstructor
  }
}
