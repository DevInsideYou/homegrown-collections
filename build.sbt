version := "0.0.1-SNAPSHOT"

organization := "devinsideyou"

scalaVersion := "2.12.4"

triggeredMessage := Watched.clearWhenTriggered

initialCommands in console := "import homegrown.collections._"

addCommandAlias("testc", ";clean;coverage;test;coverageReport")

scalacOptions ++=
  Seq(
    "-feature",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-P:continuations:enable"
  )

libraryDependencies ++=
  Seq(
    "org.scalatest" %% "scalatest" % "3.0.5" % "test", // http://www.scalatest.org
    "org.scala-lang.plugins" %% "scala-continuations-library" % "1.0.3"
  )

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.12.2" % "1.0.3")
