import sbt._

object Dependencies {
  object Test {
    val scalacheck =
      "org.scalacheck" %% "scalacheck" % "1.14.2"

    val scalatest =
      "org.scalatest" %% "scalatest" % "3.0.8"
  }
}
