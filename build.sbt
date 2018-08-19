version := "0.0.1-SNAPSHOT"

organization := "devinsideyou"

scalaVersion := "2.12.4"

triggeredMessage := Watched.clearWhenTriggered

initialCommands in console := "import homegrown.collections._"

addCommandAlias("testc", ";clean;coverage;test;coverageReport")

scalacOptions ++=
  Seq(
    "-feature",
    "-language:implicitConversions"
  )

libraryDependencies ++=
  Seq(
    "org.scalatest" %% "scalatest" % "3.0.5" % "test" // http://www.scalatest.org
  )
