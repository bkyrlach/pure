ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "pure"
  )

libraryDependencies += "org.typelevel" %% "cats-parse" % "0.3.7"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.12"
libraryDependencies += "org.jline" % "jline" % "3.21.0"