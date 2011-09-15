import sbt._
import Keys._

object build extends Build {
  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalaz",
    version := "7.1-SNAPSHOT",
    scalaVersion := "2.9.1"
  )

  lazy val scalaz = Project(
    id = "scalaz",
    base = file("."),
    settings = standardSettings,
    aggregate = Seq(core, example)
  )

  lazy val core = Project(
    id = "scalaz-core",
    base = file("core"),
    settings = standardSettings
  )

  lazy val example = Project(
    id = "scalaz-example",
    base = file("example"),
    dependencies = Seq(core),
    settings = standardSettings
  )
}
