package com.todesking.unveil

import scala.collection.mutable

abstract class AbstractLabel() extends AnyRef {
  val innerId = AbstractLabel.nextId()
  override def equals(other: Any): Boolean =
    other match {
      case l: AbstractLabel => this.innerId == l.innerId
      case _ => false
    }
  override def hashCode: Int =
    innerId.hashCode

  override def toString = s"${getClass.getName}#${innerId}"
}
object AbstractLabel {
  private[this] var _nextId = 0
  private def nextId(): Int = synchronized {
    val n = _nextId
    _nextId = _nextId + 1
    n
  }
  class Namer[A <: AbstractLabel](idPrefix: String, namePrefix: String) {
    private[this] val ids = mutable.HashMap.empty[A, Int]
    private[this] var nextId = 0

    def num(l: A): Int =
      ids.get(l) getOrElse {
        ids(l) = nextId
        nextId += 1
        ids(l)
      }
    def id(l: A): String = s"${idPrefix}${num(l)}"
    def name(l: A): String = s"${namePrefix}${num(l)}"
  }
  class Assigner[A, L <: AbstractLabel](fresh: => L) {
    private[this] val mapping = mutable.HashMap.empty[A, L]
    def apply(key: A): L =
      mapping.get(key) getOrElse {
        val l = fresh
        mapping(key) = l
        l
      }
  }
  class Merger[L <: AbstractLabel](fresh: => L) {
    private[this] val merges = new mutable.HashMap[L, mutable.Set[L]] with mutable.MultiMap[L, L]
    private[this] val cache = new mutable.HashMap[(L, L), L]

    def toMap: Map[L, Set[L]] = merges.mapValues(_.toSet).toMap

    // TODO: Is this really enough?
    def merge(l1: L, l2: L): L =
      cache.get((l1, l2)) getOrElse {
        val m = merge0(l1, l2)
        cache((l1, l2)) = m
        cache((l2, l1)) = m
        m
      }

    private[this] def merge0(l1: L, l2: L): L =
      if (l1 == l2) {
        l1
      } else if (merges.contains(l1)) {
        if (merges.contains(l2)) throw new AssertionError
        merges.addBinding(l1, l2)
        l1
      } else if (merges.contains(l2)) {
        merges.addBinding(l2, l1)
        l2
      } else if (merges.find(_._1 == l1).map(_._2.contains(l2)) getOrElse false) {
        l1
      } else if (merges.find(_._1 == l2).map(_._2.contains(l1)) getOrElse false) {
        l2
      } else {
        val m = fresh
        merges.addBinding(m, l1)
        merges.addBinding(m, l2)
        m
      }
  }

  trait NamerProvider[A <: AbstractLabel] {
    def namer(idPrefix: String, namePrefix: String): Namer[A] = new Namer(idPrefix, namePrefix)
  }
  trait AssignerProvider[A <: AbstractLabel] { self: { def fresh(): A } =>
    def assigner[B](): Assigner[B, A] = new Assigner(fresh())
  }
}

