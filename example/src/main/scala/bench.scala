package com.todesking.unveil.benchmark

import org.openjdk.jmh.annotations.{ Benchmark, State }

object Main {
  def main(args: Array[String]): Unit = {
    (-10 to 10) foreach { i =>
      val baseline = Bench.baseline(i)
      val standard = Bench.standardF(i)
      val fused = Bench.fusedF(i)
      val fuseInlined = Bench.fuseInlinedF(i)
      println((i, baseline, standard, fused, fuseInlined))
      assert(baseline == standard)
      assert(baseline == fused)
      assert(baseline == fuseInlined)
    }
  }
}

object Bench {
  def f1_(x: Int) = x + 1
  def f2_(x: Int) = x + 10.0
  def f3_(x: Double) = (x * 100).toInt
  def f4_(x: Int) = x + 1.5
  def f5_(x: Double) = x * 0.01
  def f6_(x: Double) = x - 200.0
  def f7_(x: Double) = x.toInt
  def f8_(x: Int) = x + 10
  val f1 = f1_ _
  val f2 = f2_ _
  val f3 = f3_ _
  val f4 = f4_ _
  val f5 = f5_ _
  val f6 = f6_ _
  val f7 = f7_ _
  val f8 = f8_ _

  def F(x: Int) = f8_(f7_(f6_(f5_(f4_(f3_(f2_(f1_(x))))))))

  val fastest: Int => Int = { x: Int => ((((((x + 1) + 10.0) * 100).toInt + 1.5) * 0.01) - 200.0).toInt + 10 }

  val baseline = {
    x: Int => F(F(F(F(x))))
  }

  val standardF = {
    def F = f1 andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    F andThen F andThen F andThen F
  }

  val fusedF = {
    def F = f1 andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    val FF = F andThen F andThen F andThen F
    import com.todesking.unveil.{ Transformer, Instance }
    val el = Transformer.newEventLogger
    val i = Instance.of(FF).duplicate[Function1[Int, Int]](el)
    val ti =
      Transformer.fieldFusion(i, el)
    ti.get.materialize(el).value
  }
  val fuseInlinedF = {
    def F = f1 andThen f2 andThen f3 andThen f4 andThen f5 andThen f6 andThen f7 andThen f8
    val FF = F andThen F andThen F andThen F
    import com.todesking.unveil.{ Transformer, Instance }
    val el = Transformer.newEventLogger
    val i = Instance.of(FF).duplicate[Function1[Int, Int]](el)
    val ti =
      (Transformer.fieldFusion >>> Transformer.methodInlining)(i, el)
    ti.get.materialize(el).value
  }
}

class Bench {
  @Benchmark
  def fastest(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.fastest(i) }
    x
  }

  @Benchmark
  def baseline(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.baseline(i) }
    x
  }

  @Benchmark
  def standard(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.standardF(i) }
    x
  }

  @Benchmark
  def fused(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.fusedF(i) }
    x
  }

  @Benchmark
  def fuseInlined(): Any = {
    var x = 0
    (0 until 1000).foreach { i => x += Bench.fuseInlinedF(i) }
    x
  }
}
