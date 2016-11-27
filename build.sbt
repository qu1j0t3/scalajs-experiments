name := "scalajs-experiments"

version := "0.1"

scalaVersion := "2.12.0"

enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

libraryDependencies += "org.querki" %%% "jquery-facade" % "1.0"

jsDependencies += "org.webjars" % "jquery" % "2.2.1" / "jquery.js" //minified "jquery.min.js"
