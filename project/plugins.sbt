// kubuszok plugin (bundles: sbt-git, sbt-scalafmt, sbt-scoverage, sbt-scalajs, sbt-scala-native, sbt-pgp, sbt-ide-settings, sbt-commandmatrix)
// sbt 2.0 has projectMatrix built in; sbt-welcome has no sbt2 build and is no longer bundled.
// TODO: remove after sbt-kubuszok 0.2.5
resolvers += "Maven Central Snapshots" at "https://central.sonatype.com/repository/maven-snapshots"
addSbtPlugin("com.kubuszok" % "sbt-kubuszok" % "0.2.4-4-g52226ed-SNAPSHOT")
