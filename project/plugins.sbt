ThisBuild / scalaVersion := "2.12.10"
ThisBuild / useSuperShell := false
ThisBuild / autoStartServer := false

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.4.2")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.0.6")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
