package com.todesking.unveil

class UniqueNamer() {
  private[this] var id = 0
  private[this] def nextId(): Int = {
    id += 1
    id = math.abs(id)
    id
  }

  def apply(baseNames: String*): String = {
    val prefix = baseNames
      .map(_.replaceAll("""\$[0-9]+\$$""", ""))
      .map(_.replaceAll("[^A-Za-z0-9$]", "_"))
      .mkString("__")
    prefix + "$" + nextId() + "$"
  }
}

