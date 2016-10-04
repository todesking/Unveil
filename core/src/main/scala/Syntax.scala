package com.todesking.unveil
object Syntax {
  implicit class Upcast[A](val a: A) extends AnyVal {
    def upcast[B >: A]: B = a.asInstanceOf[B]
  }

}
