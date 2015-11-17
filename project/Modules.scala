import sbt._
import Keys._
import Settings._

trait Modules {
  lazy val core = module("core", standardSettings, Seq())

  def module(
    id: String,
    settings: Seq[Def.Setting[_]],
    deps: Seq[ModuleID]
  ): Project =
    Project(id = id, base = file(id), settings = settings).settings(
      name := s"scalaz-$id"
    , libraryDependencies ++= deps
    )
}
