ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "3.5.2"

lazy val root = (project in file(".")).settings(name := "jasper_compiler")

resolvers += "jaspersoft-third-party" at "https://jaspersoft.jfrog.io/jaspersoft/third-party-ce-artifacts/"
resolvers += "jr-ce-releases" at "https://jaspersoft.jfrog.io/jaspersoft/jr-ce-releases/"
libraryDependencies += "net.sf.jasperreports" % "jasperreports" % "6.9.0"