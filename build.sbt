name := "Trains"

version := "0.1"

scalaVersion := "2.12.7"

lazy val graphCoreVersion = "1.12.3"
lazy val scalaCheckVersion = "1.13.5"
lazy val specs2Version = "4.0.3"
lazy val catsVersion = "1.0.0"

libraryDependencies ++= Seq(
  "org.scala-graph" %% "graph-core"        % graphCoreVersion,
  "org.typelevel"   %% "cats-core"         % catsVersion,
  "org.scalacheck"  %% "scalacheck"        % scalaCheckVersion % "test",
  "org.specs2"      %% "specs2-core"       % specs2Version     % "test",
  "org.specs2"      %% "specs2-scalacheck" % specs2Version     % "test"
)
