ThisBuild / organization := "jp.kazzna"
ThisBuild / name := "exteff"
ThisBuild / version := "0.1.0"

scalaVersion := "2.13.8"
scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.13" % "test",
  "org.scalatest" %% "scalatest" % "3.2.13" % "test"
)

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
