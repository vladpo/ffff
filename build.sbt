name := "apia-portal"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp" %% "core" % "1.1.2",
  "com.softwaremill.sttp" %% "async-http-client-backend-fs2" % "1.1.2",
  "com.itextpdf" % "itext7-core" % "7.1.0",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.github.nscala-time" %% "nscala-time" % "2.18.0",
  "org.scalafx" % "scalafx_2.12" % "8.0.144-R12"
)