name := "scalajs-experiments"

version := "0.1"

scalaVersion := "2.12.11"

enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
