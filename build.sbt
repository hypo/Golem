import AssemblyKeys._

assemblySettings

name := "Golem"

version := "1.0"

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases",
  "Typesafe repo" at "http://repo.typesafe.com/typesafe/repo",
  "Sonatype Snapshot" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Release" at "https://oss.sonatype.org/content/repositories/releases"
)

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.0.0",
  "org.specs2" %% "specs2" % "1.13" % "test",
  "org.apache.httpcomponents" % "httpclient" % "4.2.2",
  "org.apache.httpcomponents" % "httpmime" % "4.2.2"
)

scalacOptions ++= Seq("-feature")