import sbt._
import Keys._
import GenTypeClass._

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
    settings = standardSettings ++ Seq(
      createAllTypeClasses <<= (scalaSource in Compile, streams, typeClasses) map {
        (scalaSource, streams, typeClasses) =>
          typeClasses.flatMap { tc =>
              val typeClassSource0 = typeclassSource(tc)
              typeClassSource0.sources.map(_.createOrUpdate(scalaSource, streams.log))
          }
      },
      typeClasses := TypeClass.all
    )
  )

  lazy val example = Project(
    id = "scalaz-example",
    base = file("example"),
    dependencies = Seq(core),
    settings = standardSettings
  )

  lazy val createAllTypeClasses = TaskKey[Seq[File]]("create-all-type-classes")

  lazy val typeClasses = TaskKey[Seq[TypeClass]]("type-classes")
}
