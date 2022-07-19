scalaVersion := "2.13.8"
name := "ts-to-mls"
version := "1.0"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }

libraryDependencies += "org.scalactic" %%% "scalactic" % "3.2.12"
libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.13.8"
    )),
    name := "ts-to-mls"
  )