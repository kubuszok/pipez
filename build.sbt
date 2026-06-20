import commandmatrix.extra.*
import kubuszok.sbt._
import kubuszok.sbt.KubuszokPlugin.autoImport._

// Versions:

// sbt 2.0's build dialect drops the structural-type refinement on `new { ... }`, so members of an
// anonymous `val versions = new { ... }` object fail to resolve, and top-level objects aren't visible
// to lifted setting expressions either. Use plain top-level vals (the proven sbt-2.0 pattern).
val scala213 = "2.13.18"
val scala3 = "3.8.4"

val scalas = List(scala213, scala3)
val platforms = List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)

val hearth = "0.3.1-54-g83c3eb5-SNAPSHOT"
val kindProjector = "0.13.4"
val munit = "1.3.3"

val dev = new DevProperties(
  scala213 = Some(scala213),
  scala3 = Some(scala3),
  platforms = platforms
)

val logCrossQuotes = dev.props.getProperty("log.cross-quotes") match {
  case "true"                          => true
  case "false"                         => false
  case otherwise if otherwise.nonEmpty => otherwise
  case _                               => !isCI
}

// Common settings:

val useCrossQuotes = scalas.flatMap { scalaVersion =>
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
            libraryDependencies += compilerPlugin("com.kubuszok" %% "hearth-cross-quotes" % hearth),
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
  // sbt 2.0: %% is platform-aware (encodes Scala version + JS/Native suffix); %%% is gone.
  libraryDependencies ++= Seq(
    "com.kubuszok" %% "hearth" % hearth,
    "com.kubuszok" %% "hearth-munit" % hearth % Test,
    "org.scalameta" %% "munit" % munit % Test
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  libraryDependencies ++= foldVersion(scalaVersion.value)(
    for3 = Seq.empty,
    for2_13 = Seq(
      compilerPlugin("org.typelevel" % "kind-projector" % kindProjector cross CrossVersion.full)
    )
  ),
  resolvers += Resolver.mavenLocal
)

// sbt 2.0 + scoverage + `fork := true`: the scoverage runtime Invoker in the *forked* test JVM writes
// per-thread measurement files into `<crossTarget>/scoverage-data`, but on sbt 2.0 that directory is no
// longer pre-created in-process before the fork starts, so every instrumented test crashes with
// `FileNotFoundException: .../scoverage-data/scoverage.measurements.*`. Ensure the directory exists;
// piggy-back on Test/compile, which every test task depends on. Harmless when coverage is off.
val coverageDirFix = Seq(
  Test / compile := Def.uncached {
    val analysis = (Test / compile).value
    IO.createDirectory((Test / crossTarget).value / "scoverage-data")
    analysis
  }
)

// Scala Native 0.5.12: munit 1.3.3 pulls test-interface 0.5.12 while scalacheck 1.19.0 (transitive via
// hearth-munit) still pulls 0.5.8; the Scala Native plugin marks test-interface with a "strict" scheme,
// so the older request is reported as a binary-incompat eviction error on sbt 2.0. Coursier already
// selects the newer 0.5.12, so demote that eviction to a warning on the native axis.
val nativeEvictionFix = List(
  MatrixAction
    .ForPlatforms(VirtualAxis.native)
    .Configure(_.settings(evictionErrorLevel := sbt.util.Level.Warn))
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

// CI/test command aliases (e.g. ci-jvm-3, test-js-3), consumed by .github/workflows/ci.yml.
// sbt-welcome used to register these via `usefulTasks`, but it has no sbt 2.0 build, so we register
// them directly from the Aliases helper bundled with sbt-kubuszok.
//
// In sbt 2.0 the `test` task is incremental and machine-wide cached (it behaves like sbt 1.x's
// `testQuick`), so a CI run that begins with `clean` can still report "No tests to run" and pass
// vacuously. `testFull` is the uncached full run. Rewrite the generated `<id>/test` steps to
// `<id>/testFull` so CI always executes the whole suite.
def fullTests(command: String): String =
  command
    .split(";")
    .map { step =>
      val trimmed = step.trim
      if (trimmed.endsWith("/test")) trimmed.stripSuffix("/test") + "/testFull" else trimmed
    }
    .mkString(" ; ")

def aliasName(prefix: String, platform: String, scalaBinary: String): String =
  s"$prefix-${platform.toLowerCase}-${scalaBinary.replace('.', '_')}"

lazy val ciAliases: Seq[Def.Setting[?]] = {
  val platformNames = List("JVM", "JS", "Native")
  val scalaBinaries = List("2.13", "3")
  val perCombination = for {
    platform <- platformNames
    scalaBinary <- scalaBinaries
    setting <-
      addCommandAlias(aliasName("ci", platform, scalaBinary), fullTests(aliases.ci(platform, scalaBinary))) ++
        addCommandAlias(aliasName("test", platform, scalaBinary), fullTests(aliases.test(platform, scalaBinary)))
  } yield setting
  perCombination ++ addCommandAlias("ci-release", aliases.release)
}

lazy val pipezTestcases213 = projectMatrix
  .in(file("pipez-testcases-213"))
  .someVariations(scalas, platforms)(nativeEvictionFix *)
  .settings(
    moduleName := "pipez-testcases-213",
    name := "pipez-testcases-213"
  )
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)

lazy val pipez = projectMatrix
  .in(file("pipez"))
  .someVariations(scalas, platforms)((useCrossQuotes ++ nativeEvictionFix ++ dev.only1VersionInIDE) *)
  .enablePlugins(GitVersioning)
  .dependsOn(pipezTestcases213 % Test)
  .settings(
    moduleName := "pipez",
    name := "pipez"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(coverageDirFix *)
  .settings(publishSettings *)

lazy val pipezDsl = projectMatrix
  .in(file("pipez-dsl"))
  .someVariations(scalas, platforms)((useCrossQuotes ++ nativeEvictionFix ++ dev.only1VersionInIDE) *)
  .enablePlugins(GitVersioning)
  .settings(
    moduleName := "pipez-dsl",
    name := "pipez-dsl"
  )
  .settings(settings *)
  .settings(dependencies *)
  .settings(coverageDirFix *)
  .settings(publishSettings *)
  .dependsOn(pipez % "compile->compile;test->test")

lazy val root = project
  .in(file("."))
  .enablePlugins(GitVersioning)
  .aggregate(pipez.projectRefs *)
  .aggregate(pipezDsl.projectRefs *)
  .settings(
    name := "pipez-build"
  )
  .settings(ciAliases)
  .settings(settings *)
  .settings(publishSettings *)
  .settings(noPublishSettings *)
