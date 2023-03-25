ThisBuild / organization := "jp.kazzna"
ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "exteff",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "--deprecation",
      "--feature"
    ),
    libraryDependencies ++= Seq(
      "com.github.kazzna" % "types" % "0.1.0",
      "org.scalatest" %% "scalatest" % "3.2.15" % "test"
    ),
    Test / testOptions += Tests.Argument("-l", "org.scalatest.tags.Slow")
  )

resolvers += "jitpack" at "https://jitpack.io"
addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
