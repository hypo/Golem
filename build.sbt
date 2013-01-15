import AssemblyKeys._

assemblySettings

name := "Golem"

version := "1.0"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.0.0",
  "org.specs2" %% "specs2" % "1.13" % "test"
)

scalacOptions ++= Seq("-feature")