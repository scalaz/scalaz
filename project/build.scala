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
      createTypeClass <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
        (argTask, scalaSource in Compile, streams).map { (args: Seq[String], scalaSource: File, streams: TaskStreams) =>

          val (typeClassName, kind, extendsList) = args match {
            case List() => error("Type class name not specified")
            case List(typeClassName, KindExtractor(kind)) => (typeClassName, kind, List())
            case List(typeClassName, KindExtractor(kind), extendsList) =>
              (typeClassName, kind, extendsList.split(",").map(_.trim).toList)
          }
          
          val typeClassSource0 = typeclassSource(typeClassName, kind, extendsList)
          typeClassSource0.sources.map(_.createOrUpdate(scalaSource, streams.log))
        }
      },
      createAllTypeClasses <<= (scalaSource in Compile, streams, typeClasses) map {
        (scalaSource, streams, typeClasses) =>
          typeClasses.flatMap { tc =>
              val typeClassSource0 = typeclassSource(tc.name, tc.kind, tc.extendsList.toList.map(_.name))
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

  lazy val createTypeClass = InputKey[Seq[File]]("create-type-class",
    "Creates skeleton for a new type class. Overwrites existing files. Examples: `create-type-class * Monoid Semigroup`, `create-type-class *->* Monad Functor,Bind`")

  lazy val createAllTypeClasses = TaskKey[Seq[File]]("create-all-type-classes")

  lazy val typeClasses = TaskKey[Seq[TypeClass]]("type-classes")
}
