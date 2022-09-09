val scala2version = "2.13.8"
val scala3version = "3.2.0"

val publishSettings = Seq(
  homepage := Some(url("https://kubuszok.com")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(url("https://github.com/MateuszKubuszok/pipez"), "scm:git@github.com/MateuszKubuszok/pipez.git")
  ),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  Test / publishArtifact := false,
  pomIncludeRepository := { _ =>
    false
  },
  pomExtra :=
    <developers>
      <developer>
        <id>MateuszKubuszok</id>
        <name>Mateusz Kubuszok</name>
        <url>https://github.com/MateuszKubuszok</url>
      </developer>
    </developers>
)
val noPublishSettings = Seq(
  publish / skip := true,
  publishArtifact := false
)

val testCases = project
  .in(file("testcases"))
  .settings(
    name := "testcases",
    scalaVersion := scala2version,
    crossScalaVersions := Seq(scala2version),
    scalacOptions ++= Seq("-deprecation", "-feature", "-Xsource:3")
  )
  .settings(noPublishSettings)

val pipez = project
  .in(file("pipez"))
  .enablePlugins(GitVersioning)
  .settings(
    name := "pipez",
    organization := "com.kubuszok",
    scalaVersion := scala3version,
    crossScalaVersions := Seq(scala2version, scala3version),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("-deprecation", "-feature", "-Xsource:3", "-P:kind-projector:underscore-placeholders")
        case Some((3, 2))  => Seq("-explain", "-rewrite", "-source", "3.2-migration", "-Ykind-projector:underscores")
        case _             => Seq.empty
      }
    },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) =>
          Seq(
            "org.scala-lang" % "scala-reflect" % scalaVersion.value,
            compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
          )
        case _ => Seq.empty
      }
    },
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    scalafmtOnCompile := true
  )
  .settings(publishSettings)
  .dependsOn(testCases % "test->compile")

val root = project
  .in(file("."))
  .enablePlugins(GitVersioning)
  .settings(
    name := "pipez-build",
    crossScalaVersions := Seq(scala2version, scala3version)
  )
  .settings(publishSettings)
  .settings(noPublishSettings)
  .aggregate(pipez)

addCommandAlias("use-213", s"++ $scala2version")
addCommandAlias("use-3", s"++ $scala3version")
