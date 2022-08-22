ThisBuild / organization := "jp.kazzna"
ThisBuild / scalaVersion := "2.13.8"

val root = (project in file("."))
  .settings(
    name := "exteff",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.13" % "test"
    )
  )

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
