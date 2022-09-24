import commandmatrix.extra._
import Settings._

// Scala Native 2.13 seem to generate linking errors so we're disabling it for now

val pipez = projectMatrix
  .in(file("pipez"))
  .someVariations(
    List(scala2_13version, scala3version),
    List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)
  )(MatrixAction((s, a) => s.isScala2 && a.contains(VirtualAxis.native)).Skip)
  .enablePlugins(GitVersioning)
  .settings(name := "pipez")
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies += ("com.kubuszok" %%% "pipez-testcases" % "0.1.0").cross(CrossVersion.for3Use2_13) % Test
  )
  .settings(publishSettings: _*)

val pipezDsl = projectMatrix
  .in(file("pipez-dsl"))
  .someVariations(
    List(scala2_13version, scala3version),
    List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)
  )(MatrixAction((s, a) => s.isScala2 && a.contains(VirtualAxis.native)).Skip)
  .enablePlugins(GitVersioning)
  .settings(name := "pipez-dsl")
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies += ("com.kubuszok" %%% "pipez-testcases" % "0.1.0").cross(CrossVersion.for3Use2_13) % Test
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
