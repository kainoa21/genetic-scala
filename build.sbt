name := "genetic-scala"
version := "current"
scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xlint:_",
  "-Xfuture",
  "-Ywarn-dead-code",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)

mainClass in Global := Some("zdavep.tsp.Main")

assemblyJarName in assembly := s"tsp.jar"

addCommandAlias("dist", ";clean;compile;scalastyle;assembly")
