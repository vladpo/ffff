name := "apia-portal"

version := "0.1"

val scala = "2.12.4"
scalaVersion := scala

val monocleVersion = "1.5.0"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp" %% "core" % "1.1.2",
  "com.softwaremill.sttp" %% "async-http-client-backend-fs2" % "1.1.2",
  "com.itextpdf" % "itext7-core" % "7.1.0",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.github.nscala-time" %% "nscala-time" % "2.18.0",
  "org.scalafx" % "scalafx_2.12" % "8.0.144-R12",
  "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case _ => MergeStrategy.first
}

lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "ro.portalapia",
  scalaVersion := scala,
  test in assembly := {}
)

lazy val app = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    mainClass in assembly := Some("ro.portalapia.Main"),
    assemblyJarName := "portalapia.jar"
  )