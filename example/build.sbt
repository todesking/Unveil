scalaVersion := "2.11.7"

scalacOptions ++= Seq("-feature", "-deprecation", "-Xfatal-warnings ")

scalariformSettings

enablePlugins(JmhPlugin)
