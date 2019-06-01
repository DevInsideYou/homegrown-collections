version := "0.0.1-SNAPSHOT"

organization := "devinsideyou"

scalaVersion := "2.13.0-RC3"

triggeredMessage := Watched.clearWhenTriggered

initialCommands in console :=
  s"""|import homegrown.collections._
      |import homegrown.mathlibrary._""".stripMargin

addCommandAlias("testc", ";clean;coverage;test;coverageReport")

autoStartServer := false

scalacOptions ++=
  Seq(
    "-feature",
    "-language:implicitConversions",
    "-language:higherKinds"/*,
    "-P:continuations:enable"*/
  )

libraryDependencies ++=
  Seq(
    "org.scalatest" %% "scalatest" % "3.0.8-RC5" % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.0"
    // "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.3",
  )

autoCompilerPlugins := true

// addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.12.2" % "1.0.3")
