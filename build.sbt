name := "scalajs-experiments"

version := "0.1"

scalaVersion := "2.12.12"

enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"

libraryDependencies += "com.fazecast" % "jSerialComm" % "2.6.2"


// This is an application with a main method
//scalaJSUseMainModuleInitializer := true
