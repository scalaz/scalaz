import sbt._
import Keys._

object build extends Build {
  lazy val root = Project(
    id = "scalaz-core",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
      organization := "org.scalaz",
      name := "scalaz-core",
      version := "7.1-SNAPSHOT",
      scalaVersion := "2.9.1"
    )
  )
}
