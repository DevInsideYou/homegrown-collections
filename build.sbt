ThisBuild / organization := "com.devinsideyou"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version      := "1.0.0"

lazy val `my-project` =
  project
    .in(file("."))
    .settings(
      name := "homegrown-collections",
      libraryDependencies ++= Seq(
        Dependencies.Test.scalacheck,
        Dependencies.Test.scalatest % Test
      ),
      Compile / console / scalacOptions --= Seq(
        "-Wunused:_",
        "-Xfatal-warnings"
      ),
      Test / console / scalacOptions :=
        (Compile / console / scalacOptions).value
    )

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-Wunused:_",
  "-Xfatal-warnings"
)

ThisBuild / console / initialCommands :=
  s"""|import homegrown.collections._
      |import homegrown.mathlibrary._""".stripMargin
