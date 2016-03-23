package com.todesking.unveil

import scala.collection.mutable

object Collections {
  def newMultiMap[A, B]: mutable.MultiMap[A, B] =
    new mutable.HashMap[A, mutable.Set[B]] with mutable.MultiMap[A, B]
}
