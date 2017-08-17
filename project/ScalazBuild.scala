import sbt._
import Keys._
import org.scalajs.sbtplugin.cross.CrossProject

object Scalaz {
  val testDeps = Seq("org.scalacheck" %% "scalacheck" % "1.13.4" % "test")

  def stdSettings(prjName: String) = Seq(
    name := s"scalaz-$prjName",

    scalacOptions ++= Seq("-feature","-deprecation", "-Xlint", "-language:higherKinds",
                          "-Ydelambdafy:method", "-Ypartial-unification", "-target:jvm-1.8", "-opt:l:project",
                          "-opt-warnings"),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11)) =>
          Seq("-Ybackend:GenBCode")
        case _ =>
          Nil
      }
    },
    libraryDependencies ++= testDeps ++ Seq(
      compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
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

  implicit class ModuleHelper(p: Project) {
    def module : Project = p.in(file(p.id)).settings(stdSettings(p.id))
  }

  implicit class CrossProjectModuleHelper(p: CrossProject.Builder) {
    def module : CrossProject = p.in(file(p.jvm.id.stripSuffix("JVM"))).settings(stdSettings(p.jvm.id.stripSuffix("JVM")))
  }

}
