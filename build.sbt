name := "fundeps"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M1" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "2.0" % "test"
)