ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.11"

lazy val root = (project in file("."))
  .settings(
    name := "jasper_compiler"
  )

resolvers += "jaspersoft-third-party" at "https://jaspersoft.jfrog.io/jaspersoft/third-party-ce-artifacts/"
resolvers += "jr-ce-releases" at "https://jaspersoft.jfrog.io/jaspersoft/jr-ce-releases/"
libraryDependencies += "net.sf.jasperreports" % "jasperreports" % "6.9.0"