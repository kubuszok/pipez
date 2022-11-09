import sbt._
import sbt.Keys._
import org.scalafmt.sbt.ScalafmtPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import commandmatrix.extra._

object Settings {

  val scala2_13version = "2.13.10"
  val scala3version    = "3.2.1"

  // compiling

  val commonSettings = Seq(
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("-deprecation", "-feature", "-Xsource:3", "-Ytasty-reader", "-P:kind-projector:underscore-placeholders")
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
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M6" % Test,
    scalafmtOnCompile := true
  )

  // publishing

  val publishSettings = Seq(
    organization := "com.kubuszok",
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
}
