val scala2version = "2.13.8"
val scala3version = "3.1.3"

val testCases = project
  .in(file("testcases"))
  .settings(
    name := "testcases",
    scalaVersion := scala2version,
    scalacOptions ++= Seq("-deprecation", "-feature", "-Xsource:3")
  )

val pipez = project
  .in(file("pipez"))
  .settings(
    name := "pipez",
    scalaVersion := scala3version,
    crossScalaVersions := Seq(scala2version, scala3version),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("-deprecation", "-feature", "-Xsource:3")
        case Some((3, 1))  => Seq("-explain")
        case _             => Seq.empty
      }
    },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
        case _             => Seq.empty
      }
    },
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
    // scalafmtOnCompile := true,
  )
  .dependsOn(testCases % "test->compile")

val root = project.in(file(".")).settings(name := "pipez-build").aggregate(pipez)

addCommandAlias("use-213", s"++ $scala2version")
addCommandAlias("use-3", s"++ $scala3version")
