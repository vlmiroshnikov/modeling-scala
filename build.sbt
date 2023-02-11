import Settings._

ThisBuild / scalaVersion := Versions.dotty

lazy val root = project
  .in(file("."))
  .settings(scalaVersion := Versions.dotty)
  .aggregate(modeling)
  .settings(
    publish         := {},
    publishLocal    := {},
    publishArtifact := false,
    publish / skip  := true
  )

lazy val modeling = project
  .in(file("modeling"))
  .settings(
    name                 := "modeling",
    scalaVersion         := Versions.dotty,
    libraryDependencies ++= cats ++ catsEffect ++ munit ++ munitEffect
  )
