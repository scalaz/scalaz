import sbt._
import Keys._

import pl.project13.scala.sbt.JmhPlugin

object Scalaz extends Build {
  val testDeps = Seq("org.scalacheck" %% "scalacheck" % "1.13.0" % "test")

  def module(prjName: String) = Project(
    id = prjName,
    base = file(prjName)).settings(
    name := s"scalaz-$prjName",
    scalaVersion := "2.11.8",
    scalacOptions ++= Seq("-feature","-deprecation", "-Xlint", "-language:higherKinds",
                          "-Ybackend:GenBCode", "-Ydelambdafy:method", "-target:jvm-1.8"),
    libraryDependencies ++= testDeps ++ Seq(
      "org.scala-lang.modules" %% "scala-java8-compat" % "0.7.0",
      compilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),
      compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.1.0" cross CrossVersion.full)
    )
  )

  lazy val root = Project(
    id = "root",
    base = file(".")
  ).settings(
    scalaVersion := "2.11.8"
  ).aggregate ( baze
              , meta
              , benchmarks )

  lazy val baze         = module("base")
    .dependsOn( meta )

  lazy val benchmarks   = module("benchmarks")
    .dependsOn( baze )
    .enablePlugins(JmhPlugin)
    .settings(
      libraryDependencies ++=
        Seq ( "org.scala-lang" % "scala-reflect" % scalaVersion.value
            , "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
            , "org.scalaz" %% "scalaz-core" % "7.2.1"
            , "org.typelevel" %% "cats" % "0.5.0" )
    )

  lazy val meta         = module("meta")
    .settings(
      libraryDependencies ++=
        Seq ( "org.scala-lang" % "scala-reflect" % scalaVersion.value
            , "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided" )
    )
}
