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
    settings = standardSettings ++ Seq(
      createTypeClass <<= inputTask { (argTask: TaskKey[Seq[String]]) =>
        (argTask, scalaSource in Compile, streams).map { (args: Seq[String], scalaSource: File, streams: TaskStreams) =>
          val (typeClassName, extendsList) = args match {
            case List() => error("Type class name not specified")
            case List(typeClassName) => (typeClassName, List())
            case List(typeClassName, extendsList) => (typeClassName, extendsList.split(",").map(_.trim).toList)
          }
          
          val typeClassSource0 = GenTypeClass.typeclassSource(typeClassName, extendsList)
          def write(source: GenTypeClass.SourceFile) {
            val outFile = source.file(scalaSource)
            streams.log.info("Writing %s".format(outFile))
            streams.log.debug("Contents: %s".format(source.source))
            IO.write(outFile, source.source)
          }
          typeClassSource0.sources.foreach(write)
          typeClassSource0.sources.map(_.file(scalaSource))
        }
      }
    )
  )

  lazy val example = Project(
    id = "scalaz-example",
    base = file("example"),
    dependencies = Seq(core),
    settings = standardSettings
  )

  lazy val createTypeClass = InputKey[Seq[File]]("create-type-class",
    "Creates skeleton for a new type class. Overwrites existing files. Example `create-type-class Monad Functor,Bind`")
}
