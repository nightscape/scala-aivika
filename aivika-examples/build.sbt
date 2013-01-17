name := "aivika-examples"

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "continuations" % "2.10.0")

scalacOptions += "-P:continuations:enable"

libraryDependencies ++= Seq(
 "org.scala-lang" % "scala-library" % "2.10.0",
 "org.scala-lang.plugins" % "continuations" % "2.10.0" ,
 "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
 "junit" % "junit" % "4.8.1"  % "test"
)
