name := "TrainApp"

version := "0.1"

scalaVersion := "2.12.7"

lazy val graphCoreVersion = "1.12.3"
lazy val scalaCheckVersion = "1.13.5"
lazy val specs2Version = "4.0.3"
lazy val catsVersion = "1.0.0"
lazy val akkaVersion = "2.5.23"
lazy val akkHttpVersion = "10.1.8"
lazy val leveldbjniVersion = "1.8"

libraryDependencies ++= Seq(
  "org.scala-graph"   %% "graph-core"        % graphCoreVersion,
  "org.typelevel"     %% "cats-core"         % catsVersion,
  "com.typesafe.akka" %% "akka-actor"        % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence-typed"  % akkaVersion,
  "com.typesafe.akka" %% "akka-stream"  % akkaVersion,
  "com.typesafe.akka" %% "akka-http-core"  % akkHttpVersion,
  "com.typesafe.akka" %% "akka-http"  % akkHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkHttpVersion,
  "org.iq80.leveldb"  % "leveldb" % "0.7",
  "org.fusesource.leveldbjni" % "leveldbjni-all" % leveldbjniVersion,
  "org.scalacheck"    %% "scalacheck"        % scalaCheckVersion % "test",
  "org.specs2"        %% "specs2-core"       % specs2Version     % "test",
  "org.specs2"        %% "specs2-scalacheck" % specs2Version     % "test"
)
