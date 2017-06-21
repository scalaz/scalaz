import sbt._
import Keys._

object Scalaz extends Build {
  val testDeps = Seq("org.scalacheck" %% "scalacheck" % "1.13.4" % "test")

  def module(prjName: String) = Project(
    id = prjName,
    base = file(prjName)).settings(
    name := s"scalaz-$prjName",
    scalaVersion := "2.11.11",
    scalacOptions ++= Seq("-feature","-deprecation", "-Xlint", "-language:higherKinds",
                          "-Ydelambdafy:method", "-Ypartial-unification", "-target:jvm-1.8"),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11)) =>
          Seq("-Ybackend:GenBCode")
        case _ =>
          Nil
      }
    },
    libraryDependencies ++= testDeps ++ Seq(
      compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
    ) ++ {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11)) =>
          Seq(
            "org.scala-lang.modules" %% "scala-java8-compat" % "0.7.0"
          )
        case _ =>
          Nil
      }
    },
    incOptions ~= (_.withLogRecompileOnMacro(false))
  )
}
