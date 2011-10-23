import sbt._
import Keys._
import GenTypeClass._

object build extends Build {
  lazy val standardSettings: Seq[Project.Setting[_]] = Defaults.defaultSettings ++ Seq[Project.Setting[_]](
    organization := "org.scalaz",
    version := "7.1-SNAPSHOT",
    scalaVersion := "2.9.1",

    createAllTypeClasses <<= (scalaSource in Compile, streams, typeClasses) map {
      (scalaSource, streams, typeClasses) =>
        typeClasses.flatMap {
          tc =>
            val typeClassSource0 = typeclassSource(tc)
            typeClassSource0.sources.map(_.createOrUpdate(scalaSource, streams.log))
        }
    },
    typeClasses := Seq()
  )

  lazy val scalaz = Project(
    id = "scalaz",
    base = file("."),
    settings = standardSettings,
    aggregate = Seq(core, effect, iteratee, example)
  )

  lazy val core = Project(
    id = "scalaz-core",
    base = file("core"),
    settings = standardSettings ++ Seq(
      typeClasses := TypeClass.core
    )
  )

  lazy val concurrent = Project(
    id = "scalaz-concurrent",
    base = file("concurrent"),
    settings = standardSettings ++ Seq(
      typeClasses := TypeClass.concurrent
    ),
    dependencies = Seq(core)
  )

  lazy val effect = Project(
    id = "scalaz-effect",
    base = file("effect"),
    settings = standardSettings,
    dependencies = Seq(concurrent)
  )

  lazy val iteratee = Project(
    id = "scalaz-iteratee",
    base = file("iteratee"),
    settings = standardSettings,
    dependencies = Seq(effect)
  )

  lazy val example = Project(
    id = "scalaz-example",
    base = file("example"),
    dependencies = Seq(core, iteratee, concurrent),
    settings = standardSettings
  )

  lazy val createAllTypeClasses = TaskKey[Seq[File]]("create-all-type-classes")

  lazy val typeClasses = TaskKey[Seq[TypeClass]]("type-classes")
}
