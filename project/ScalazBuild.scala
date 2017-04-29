import sbt._
import Keys._

import pl.project13.scala.sbt.JmhPlugin

object Scalaz extends Build {
  val testDeps = Seq("org.scalacheck" %% "scalacheck" % "1.13.4" % "test")

  def module(prjName: String) = Project(
    id = prjName,
    base = file(prjName)).settings(
    name := s"scalaz-$prjName",
    scalaVersion := "2.11.8",
    scalacOptions ++= Seq("-feature","-deprecation", "-Xlint", "-language:higherKinds",
                          "-Ydelambdafy:method", "-target:jvm-1.8"),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11)) =>
          Seq("-Ybackend:GenBCode")
        case _ =>
          Seq("-Ypartial-unification")
      }
    },
    libraryDependencies ++= testDeps ++ Seq(
      compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
    ) ++ {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11)) =>
          Seq(
            "org.scala-lang.modules" %% "scala-java8-compat" % "0.7.0",
            compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.1.0" cross CrossVersion.full)
          )
        case _ =>
          Nil
      }
    }
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
            , "org.scalaz" %% "scalaz-core" % "7.2.7")
    )

  lazy val meta         = module("meta")
    .settings(
      libraryDependencies ++=
        Seq ( "org.scala-lang" % "scala-reflect" % scalaVersion.value
            , "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided" )
    )

  lazy val tutorial     = module("tutorial")
    .dependsOn( baze )
    .settings(tut.Plugin.tutSettings)
}
