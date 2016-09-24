scalaVersion := "2.11.7"

organization := "com.todesking"

name := "unveil"

resolvers += "com.todesking" at "http://todesking.github.io/mvn/"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.javassist" % "javassist" % "3.20.0-GA"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "com.todesking" %% "scala-pp" % "0.0.4"

scalacOptions ++= Seq("-feature", "-deprecation")

testOptions in Test += Tests.Argument("-oFI")

scalariformSettings

fork in test := true
