scalaVersion := "2.11.7"

lazy val root = project aggregate(core, example)

lazy val core = project

lazy val example =  project dependsOn core
