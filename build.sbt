val scala2version = "2.13.8"
val scala3version = "3.1.3"

val pipez = project
  .in(file("pipez"))
  .settings(
    name := "pipez",
    scalaVersion := scala2version,
    scalacOptions ++= Seq(
      "-feature",
    ),
    crossScalaVersions := Seq(scala2version, scala3version),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
        case _             => Seq.empty
      }
    },
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )

val root = project.in(file(".")).settings(name := "pipez-build").aggregate(pipez)
