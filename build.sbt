ThisBuild / scalaVersion := "2.13.8"

val diffTests = taskKey[Unit]("")

diffTests := (Def.task{
  (ts2mlsJVM / Test / test).value
} triggeredBy (ts2mlsJS / Test / test)).value

lazy val root = project.in(file("."))
  .aggregate(ts2mlsJS, ts2mlsJVM)
  .settings(
    publish := {},
    publishLocal := {},
  )

lazy val ts2mls = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "ts2mls",
    scalaVersion := "2.13.8",
    libraryDependencies += "com.lihaoyi" %% "fansi" % "0.2.14",
  )
  .jvmSettings(
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.8.0",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test"
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test"
  )

lazy val ts2mlsJS = ts2mls.js
lazy val ts2mlsJVM = ts2mls.jvm