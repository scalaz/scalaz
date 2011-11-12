import sbt._
import Keys._
import GenTypeClass._
import Project.Setting

object build extends Build {
  type Sett = Project.Setting[_]

  lazy val standardSettings: Seq[Sett] = Defaults.defaultSettings ++ Seq[Sett](
    organization := "org.scalaz",
    version := "7.1-SNAPSHOT",
    scalaVersion := "2.9.1",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-Ydependent-method-types"),

    genTypeClasses <<= (scalaSource in Compile, streams, typeClasses) map {
      (scalaSource, streams, typeClasses) =>
        typeClasses.flatMap {
          tc =>
            val typeClassSource0 = typeclassSource(tc)
            typeClassSource0.sources.map(_.createOrUpdate(scalaSource, streams.log))
        }
    },
    typeClasses := Seq(),
    genToSyntax <<= (typeClasses) map {
      (tcs: Seq[TypeClass]) =>
        val objects = tcs.map(tc => "object %s extends To%sSyntax".format(Util.initLower(tc.name), tc.name)).mkString("\n")
        val all = "object all extends " + tcs.map(tc => "To%sSyntax".format(tc.name)).mkString(" with ")
        objects + "\n\n" + all
    }

  )

  lazy val scalaz = Project(
    id = "scalaz",
    base = file("."),
    settings = standardSettings,
    aggregate = Seq(core, concurrent, effect, iteratee, example)
  )

  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = standardSettings ++ Seq[Sett](
      name := "scalaz-core",
      typeClasses := TypeClass.core
    )
  )

  lazy val concurrent = Project(
    id = "concurrent",
    base = file("concurrent"),
    settings = standardSettings ++ Seq[Sett](
      name := "scalaz-concurrent",
      typeClasses := TypeClass.concurrent
    ),
    dependencies = Seq(core)
  )

  lazy val effect = Project(
    id = "effect",
    base = file("effect"),
    settings = standardSettings ++ Seq[Sett](
      name := "scalaz-effect"
    )
  )

  lazy val iteratee = Project(
    id = "iteratee",
    base = file("iteratee"),
    settings = standardSettings ++ Seq[Sett](
      name := "scalaz-iteratee"
    ),
    dependencies = Seq(effect)
  )

  lazy val example = Project(
    id = "example",
    base = file("example"),
    dependencies = Seq(core, iteratee, concurrent),
    settings = standardSettings ++ Seq[Sett](
      name := "scalaz-example"
    )
  )

  lazy val genTypeClasses = TaskKey[Seq[File]]("gen-type-classes")

  lazy val typeClasses = TaskKey[Seq[TypeClass]]("type-classes")

  lazy val genToSyntax = TaskKey[String]("gen-to-syntax")
}
