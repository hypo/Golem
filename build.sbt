import AssemblyKeys._

assemblySettings

name := "Golem"

version := "1.0"

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases",
  "Typesafe repo" at "http://repo.typesafe.com/typesafe/repo",
  "Sonatype Snapshot" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Release" at "https://oss.sonatype.org/content/repositories/releases",
  "Spray Repo" at "http://repo.spray.io/"
)

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.0.0",
  "org.specs2" %% "specs2" % "1.13" % "test",
  "org.slf4j" % "slf4j-api" % "1.6.4",
  "io.spray" % "spray-json_2.10" % "1.2.3",
//  "org.slf4j" % "slf4j-nop" % "1.6.4",
//  "ch.qos.logback" % "logback-classic" % "1.0.9",
  "com.typesafe" % "slick_2.10" % "1.0.0-RC1",
  "mysql" % "mysql-connector-java" % "5.1.22",
  "net.databinder.dispatch" %% "dispatch-core" % "0.9.5",
  "org.apache.httpcomponents" % "httpclient" % "4.2.2",
  "org.apache.httpcomponents" % "httpmime" % "4.2.2"
)

scalacOptions ++= Seq("-feature")