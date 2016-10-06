package com.todesking.unveil

object Algorithm {

  def mapZip[A, B, C](a: Map[A, B], b: Map[A, C]): (Map[A, (B, C)], Map[A, B], Map[A, C]) = {
    val aOnly = a.keySet -- b.keySet
    val bOnly = b.keySet -- a.keySet
    val common = a.keySet -- aOnly
    Tuple3(
      common.map { k => k -> (a(k) -> b(k)) }.toMap,
      a.filterKeys(aOnly),
      b.filterKeys(bOnly)
    )
  }

  def sharedNothingUnion[A, B, C <: B, D <: B](m1: Map[A, C], m2: Map[A, D]): Option[Map[A, B]] = {
    val union = m1 ++ m2
    if (m1.size + m2.size > union.size) None
    else Some(union)
  }

  def tsort[A, B](in: Seq[A])(labelOf: A => B)(depsOf: A => Set[B]): Seq[A] =
    tsort0(in.map { i => (i, labelOf(i), depsOf(i)) }, Set.empty, Seq.empty)

  private[this] def tsort0[A, B](in: Seq[(A, B, Set[B])], deps: Set[B], sorted: Seq[A]): Seq[A] =
    if (in.isEmpty) {
      sorted
    } else {
      val (nodep, dep) = in.partition { case (a, b, bs) => bs.forall(deps.contains) }
      if (nodep.isEmpty) throw new IllegalArgumentException(s"Cyclic reference found: ${dep}")
      tsort0(dep, deps ++ nodep.map(_._2), sorted ++ nodep.map(_._1))
    }
}
