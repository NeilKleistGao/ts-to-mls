scalaVersion := "2.13.8"
name := "ts-to-mls"
version := "1.0"

enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.13.8"
    )),
    name := "ts-to-mls"
  )