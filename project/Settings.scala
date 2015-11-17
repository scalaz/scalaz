import sbt._
import Keys._

object Settings {
  val scalacFlags = Seq(
    "-deprecation"
  , "-encoding", "UTF-8"
  , "-target:jvm-1.8"
  , "-feature"
  , "-language:_"
  , "-unchecked"
  , "-Xfatal-warnings"
  , "-Xfuture"
  , "-Xlint"
  , "-Yno-adapted-args"
  , "-Yno-imports"
  , "-Ywarn-dead-code"
  , "-Ywarn-numeric-widen"
  , "-Ywarn-value-discard"
  , "-Ywarn-unused"
  , "-Ywarn-unused-import"
  , "-Xlog-free-terms"
  )

  lazy val standardSettings =
    Seq[Def.Setting[_]](
      organization := "org.scalaz"
    , scalaVersion := "2.11.7"
    , scalacOptions ++= scalacFlags
    , javacOptions ++= Seq("-encoding", "UTF-8")
    , resolvers ++= Seq(Resolver.bintrayRepo("non", "maven"))
    , addCompilerPlugin("org.spire-math" % "kind-projector" % "0.6.3" cross CrossVersion.binary)
    )
}
