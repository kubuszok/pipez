import commandmatrix.extra._
import Settings._

// 2.13 only since apparently my way of doing Java Beans (@BeanProperty) have slightly different behavior in 3.x
val testCases = projectMatrix
  .in(file("testcases"))
  .allVariations(
    List(scala2_13version),
    List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)
  )
  .settings(name := "testcases")
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(noPublishSettings)

val pipez = projectMatrix
  .in(file("pipez"))
  .allVariations(
    List(scala2_13version, scala3version),
    List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)
  )
  .enablePlugins(GitVersioning)
  .settings(name := "pipez")
  .settings(commonSettings: _*)
  .settings(publishSettings: _*)
  .dependsOn(testCases % "test->compile")
  .settings(
    excludeDependencies ++= Seq(
      ExclusionRule().withName("testcases_3"),
      ExclusionRule().withName("testcases_sjs1_3"),
      ExclusionRule().withName("testcases_native0.4_3")
    )
  )

val pipezDsl = projectMatrix
  .in(file("pipez-dsl"))
  .allVariations(
    List(scala2_13version, scala3version),
    List(VirtualAxis.jvm, VirtualAxis.js, VirtualAxis.native)
  )
  .enablePlugins(GitVersioning)
  .settings(name := "pipez-dsl")
  .settings(commonSettings: _*)
  .settings(publishSettings: _*)
  .dependsOn(testCases % "compile->test", pipez)
  .settings(
    excludeDependencies ++= Seq(
      ExclusionRule().withName("testcases_3"),
      ExclusionRule().withName("testcases_sjs1_3"),
      ExclusionRule().withName("testcases_native0.4_3")
    )
  )

val root = project
  .in(file("."))
  .enablePlugins(GitVersioning)
  .settings(name := "pipez-build")
  .settings(commonSettings: _*)
  .settings(publishSettings: _*)
  .settings(noPublishSettings: _*)
  .aggregate(pipez.projectRefs ++ pipezDsl.projectRefs: _*)
