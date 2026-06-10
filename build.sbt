import sbtwelcome.UsefulTask
import commandmatrix.extra.*
import kubuszok.sbt._
import kubuszok.sbt.KubuszokPlugin.autoImport._

// Versions:

val versions = new {
  val scala213 = "2.13.18"
  val scala3   = "3.8.4"

  val scalas    = List(scala213, scala3)
  val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

  val hearth       = "0.3.0-62-g29ea430-SNAPSHOT"
  val kindProjector = "0.13.4"
  val munit        = "1.1.0"
}

val dev = new DevProperties(
  scala213 = Some(versions.scala213),
  scala3 = Some(versions.scala3),
  platforms = versions.platforms
)

val logCrossQuotes = dev.props.getProperty("log.cross-quotes") match {
  case "true"                          => true
  case "false"                         => false
  case otherwise if otherwise.nonEmpty => otherwise
  case _                               => !isCI
}

// Common settings:

val useCrossQuotes = versions.scalas.flatMap { scalaVersion =>
  foldVersion(scalaVersion)(
    for2_13 = List(
      MatrixAction
        .ForScala(_.isScala2)
        .Configure(_.settings(scalacOptions += s"-Xmacro-settings:hearth.cross-quotes.logging=$logCrossQuotes"))
    ),
    for3 = List(
      MatrixAction
        .ForScala(_.isScala3)
        .Configure(
          _.settings(
            libraryDependencies += compilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % versions.hearth),
            scalacOptions += s"-P:hearth.cross-quotes:logging=$logCrossQuotes"
          )
        )
    )
  )
}

val settings = Seq(
  scalacOptions ++= foldVersion(scalaVersion.value)(
    for3 = Seq(
      // format: off
      "-encoding", "UTF-8",
      "-release", "17",
      "-rewrite",
      "-source", "3.3-migration",
      // format: on
      "-unchecked",
      "-deprecation",
      "-explain",
      "-explain-types",
      "-feature",
      "-no-indent",
      "-Wconf:msg=Unreachable case:s",
      "-Wconf:msg=Missing symbol position:s",
      "-Wconf:msg=discarded non-Unit value:s",
      "-Wconf:msg=unused explicit parameter:s",
      "-Wconf:msg=Infinite loop in function body:s",
      "-Werror",
      "-Wnonunit-statement",
      "-Wunused:privates",
      "-Wunused:locals",
      "-Wunused:explicits",
      "-Wunused:implicits",
      "-Wunused:params",
      "-Wvalue-discard",
      // "-Xcheck-macros", // Disabled: type-erased code gen uses Any which is correct at runtime but fails strict checking
      "-Xkind-projector:underscores"
    ),
    for2_13 = Seq(
      // format: off
      "-encoding", "UTF-8",
      "-release", "11",
      // format: on
      "-unchecked",
      "-deprecation",
      "-explaintypes",
      "-feature",
      "-language:higherKinds",
      "-Wconf:cat=scala3-migration:s",
      "-Wconf:msg=The outer reference in this type test cannot be checked at run time:s",
      "-Wconf:cat=lint-type-parameter-shadow:s",
      "-Wconf:cat=unused-pat-vars:s",
      "-Wconf:cat=unused-locals:s",
      "-Wconf:cat=unused-imports:s",
      "-Wconf:msg=unreachable code:s",
      "-Wconf:msg=key not found:s",
      "-Wunused:patvars",
      "-Xfatal-warnings",
      "-Xlint:adapted-args",
      "-Xlint:delayedinit-select",
      "-Xlint:doc-detached",
      "-Xlint:inaccessible",
      "-Xlint:infer-any",
      "-Xlint:nullary-unit",
      "-Xlint:option-implicit",
      "-Xlint:package-object-classes",
      "-Xlint:poly-implicit-overload",
      "-Xlint:private-shadow",
      "-Xlint:stars-align",
      "-Xlint:type-parameter-shadow",
      "-Xsource:3",
      "-Yrangepos",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused:locals",
      "-Ywarn-unused:imports",
      "-Ywarn-macros:after",
      "-Xsource-features:eta-expand-always",
      "-Ytasty-reader"
    )
  )
)

val dependencies = Seq(
  libraryDependencies ++= Seq(
    "com.kubuszok" %%% "hearth"       % versions.hearth,
    "com.kubuszok" %%% "hearth-munit" % versions.hearth % Test,
    "org.scalameta" %%% "munit"       % versions.munit  % Test
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  libraryDependencies ++= foldVersion(scalaVersion.value)(
    for3 = Seq.empty,
    for2_13 = Seq(
      compilerPlugin("org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full)
    )
  ),
  resolvers += Resolver.mavenLocal
)

val publishSettings = Seq(
  organization := "com.kubuszok",
  homepage := Some(url("https://kubuszok.com")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  scmInfo := Some(
    ScmInfo(url("https://github.com/MateuszKubuszok/pipez"), "scm:git@github.com/MateuszKubuszok/pipez.git")
  ),
  developers := List(
    Developer("MateuszKubuszok", "Mateusz Kubuszok", "", url("https://kubuszok.com"))
  ),
  projectType := ProjectType.ScalaLibrary
)

val noPublishSettings =
  Seq(projectType := ProjectType.NonPublished)

// Modules:

lazy val aliases = new Aliases(
  published = Seq(pipez, pipezDsl),
  testOnly = Seq.empty,
  compileOnly = Seq.empty
)

lazy val pipezTestcases213 = projectMatrix
  .in(file("pipez-testcases-213"))
  .someVariations(versions.scalas, versions.platforms)()
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "pipez-testcases-213",
    name := "pipez-testcases-213"
  )
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)

lazy val pipez = projectMatrix
  .in(file("pipez"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .enablePlugins(GitVersioning)
  .disablePlugins(WelcomePlugin)
  .dependsOn(pipezTestcases213 % Test)
  .settings(
    moduleName := "pipez",
    name := "pipez"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)

lazy val pipezDsl = projectMatrix
  .in(file("pipez-dsl"))
  .someVariations(versions.scalas, versions.platforms)((useCrossQuotes ++ dev.only1VersionInIDE) *)
  .enablePlugins(GitVersioning)
  .disablePlugins(WelcomePlugin)
  .settings(
    moduleName := "pipez-dsl",
    name := "pipez-dsl"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(publishSettings *)
  .dependsOn(pipez % "compile->compile;test->test")

lazy val root = project
  .in(file("."))
  .enablePlugins(GitVersioning, WelcomePlugin)
  .aggregate(pipez.projectRefs *)
  .aggregate(pipezDsl.projectRefs *)
  .settings(
    name := "pipez-build",
    logo :=
      s"""Pipez ${version.value} build for (${versions.scala213}, ${versions.scala3}) x (JVM, Scala.js, Scala Native)
         |
         |This build uses sbt-projectmatrix with sbt-kubuszok:
         | - Scala JVM adds no suffix to a project name seen in build.sbt
         | - Scala.js adds the "JS" suffix to a project name seen in build.sbt
         | - Scala Native adds the "Native" suffix to a project name seen in build.sbt
         | - Scala 2.13 adds no suffix to a project name seen in build.sbt
         | - Scala 3 adds the suffix "3" to a project name seen in build.sbt""".stripMargin,
    usefulTasks := aliases.usefulTasks()
  )
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
