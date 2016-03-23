package com.todesking.unveil

object Parsers extends scala.util.parsing.combinator.RegexParsers {
  def parseTypeRef(src: String, cl: ClassLoader): TypeRef.Public =
    parseAll(typeRef(cl).all, src).get

  def parseMethodDescriptor(src: String, cl: ClassLoader): MethodDescriptor =
    parseAll(methodDescriptor(cl).all, src).get

  def parseFieldDescriptor(src: String, cl: ClassLoader): FieldDescriptor =
    parseAll(fieldDescriptor(cl).all, src).get

  case class typeRef(classLoader: ClassLoader) {
    val refPat = """L([^;]+);""".r
    lazy val all: Parser[TypeRef.Public] = "B|Z|C|S|I|F|J|D|V|L[^;]+;".r ^^ {
      case "B" => TypeRef.Byte
      case "Z" => TypeRef.Boolean
      case "C" => TypeRef.Char
      case "S" => TypeRef.Short
      case "I" => TypeRef.Int
      case "F" => TypeRef.Float
      case "J" => TypeRef.Long
      case "D" => TypeRef.Double
      case "V" => TypeRef.Void
      case `refPat`(ref) =>
        val cName = ref.replaceAll("/", ".")
        val klass = (if (classLoader == null) ClassLoader.getSystemClassLoader else classLoader).loadClass(cName)
        TypeRef.Reference(ClassRef.of(klass))
    }
  }

  case class methodDescriptor(classLoader: ClassLoader) {
    lazy val all = args ~ tpe ^^ { case args ~ ret => MethodDescriptor(ret, args) }
    lazy val args = ('(' ~> rep(tpe)) <~ ')'
    lazy val tpe = typeRef(classLoader).all
  }

  case class fieldDescriptor(classLoader: ClassLoader) {
    lazy val all = typeRef(classLoader).all.map { tr => FieldDescriptor(tr) }
  }
}
