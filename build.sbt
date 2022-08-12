ThisBuild / scalaVersion := "2.13.8"

/* add this part to the mlscript build.sbt! */
/* ------------------------------------------- */
// val diffTests = taskKey[Unit]("")

// diffTests := (Def.task{
//   (ts2mlsJVM / Test / test).value
// } triggeredBy (ts2mlsJS / Test / test)).value
/* ------------------------------------------- */

lazy val root = project.in(file("."))
  .aggregate(ts2mlsJS, ts2mlsJVM)
  .settings(
    publish := {},
    publishLocal := {},
  )

lazy val ts2mls = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "ts2mls",
    scalaVersion := "2.13.8"
  )
  .jvmSettings()
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test"
  )

lazy val ts2mlsJS = ts2mls.js
lazy val ts2mlsJVM = ts2mls.jvm // don't use this part unless in mlscript project