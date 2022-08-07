scalaVersion := "2.13.8"
name := "ts-to-mls"
version := "1.0"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test"

lazy val root = (project in file(".")).
  settings(
    scalaVersion := "2.13.8",
    name := "ts-to-mls"
  )