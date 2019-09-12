val scala_version = "2.13.0"

lazy val root = (project in file(".")).settings(
  name := "CaptureTheFlag",
  version := "0.1",
  scalaVersion := scala_version,
  mainClass in Compile := Some("jade.Boot"),
  mainClass in assembly := Some("jade.Boot")
)

resolvers += "Jade" at "https://jade.tilab.com/maven/"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "com.tilab.jade" % "jade" % "4.3.2"
)
