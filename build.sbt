import commandmatrix.extra._
import Settings._

val testCasesVersion = "0.1.0-SNAPSHOT"

// Scala Native 2.13 seem to generate linking errors so we're disabling it for now
val skip2_13Native = MatrixAction((s, a) => s.isScala2 && a.contains(VirtualAxis.native)).Skip

// IDEs don't like projects which share sources
val ideScala = scala2_13version
Global / excludeLintKeys += ideSkipProject
val only1JvmScalaInIde =
  MatrixAction.ForPlatforms(VirtualAxis.jvm).Configure(_.settings(ideSkipProject := (scalaVersion.value != ideScala)))
val noJsNoNativeInIde =
  MatrixAction.ForPlatforms(VirtualAxis.js, VirtualAxis.native).Configure(_.settings(ideSkipProject := true))

val pipez = projectMatrix
  .in(file("pipez"))
  .someVariations(
    List(scala2_13version, scala3version),
    List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)
  )(skip2_13Native, only1JvmScalaInIde, noJsNoNativeInIde)
  .enablePlugins(GitVersioning)
  .settings(name := "pipez")
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      ("com.kubuszok" %%% "pipez-testcases" % testCasesVersion).cross(CrossVersion.for3Use2_13) % Test,
      ("com.kubuszok" %%% "pipez-testcases-scala3" % testCasesVersion).cross(CrossVersion.for2_13Use3) % Test,
    )
  )
  .settings(publishSettings: _*)

val pipezDsl = projectMatrix
  .in(file("pipez-dsl"))
  .someVariations(
    List(scala2_13version, scala3version),
    List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)
  )(skip2_13Native, only1JvmScalaInIde, noJsNoNativeInIde)
  .enablePlugins(GitVersioning)
  .settings(name := "pipez-dsl")
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      ("com.kubuszok" %%% "pipez-testcases" % testCasesVersion).cross(CrossVersion.for3Use2_13) % Test,
      ("com.kubuszok" %%% "pipez-testcases-scala3" % testCasesVersion).cross(CrossVersion.for2_13Use3) % Test,
    )
  )
  .settings(publishSettings: _*)
  .dependsOn(pipez)

val root = project
  .in(file("."))
  .enablePlugins(GitVersioning)
  .settings(name := "pipez-build")
  .settings(commonSettings: _*)
  .settings(publishSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(pipez.projectRefs ++ pipezDsl.projectRefs: _*)
