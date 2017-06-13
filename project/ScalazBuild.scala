import sbt._
import Keys._

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
}
