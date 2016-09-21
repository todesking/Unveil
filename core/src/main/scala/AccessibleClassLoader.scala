package com.todesking.unveil

class AccessibleClassLoader(parent: ClassLoader) extends ClassLoader(parent) {
  def registerClass(name: String, bytes: Array[Byte]): Unit = {
    defineClass(name, bytes, 0, bytes.size)
  }
}
