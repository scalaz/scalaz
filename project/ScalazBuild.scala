import sbt._
import Keys._
import org.scalajs.sbtplugin.cross.CrossProject

object Scalaz {
  val testDeps = Seq("org.scalacheck" %% "scalacheck" % "1.13.4" % "test")
  val compileOnlyDeps = Seq("com.github.ghik" %% "silencer-lib" % "0.5" % "provided")

  private val stdOptions = Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-explaintypes",
    "-Yrangepos",
    "-feature",
    "-Xfuture",
    "-Ypartial-unification",
    "-language:higherKinds", "-language:existentials",
    "-unchecked",
    "-Yno-adapted-args",
    "-opt-warnings",
    "-Xlint:_,-type-parameter-shadow",
    "-Ywarn-dead-code",
    "-Ywarn-extra-implicit",
    "-Ywarn-inaccessible",
    "-Ywarn-infer-any",
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused:_,-imports",
    "-Ywarn-value-discard",
    "-opt:l:inline",
    "-opt-inline-from:<source>"
  )

  def stdSettings(prjName: String) = Seq(
    name := s"scalaz-$prjName",

    scalacOptions := stdOptions,
    scalacOptions in (Compile, compile) ++=
      Seq("-Ywarn-unused:imports",
          "-Xfatal-warnings"
      ),
    libraryDependencies ++= compileOnlyDeps ++ testDeps ++ Seq(
      compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
      compilerPlugin("com.github.tomasmikula" %% "pascal" % "0.1"),
      compilerPlugin("com.github.ghik" %% "silencer-plugin" % "0.5")
    ),
    incOptions ~= (_.withLogRecompileOnMacro(false))
  )

  implicit class ModuleHelper(p: Project) {
    def module : Project = p.in(file(p.id)).settings(stdSettings(p.id))
  }

  implicit class CrossProjectModuleHelper(p: CrossProject.Builder) {
    def module : CrossProject = p.in(file(p.jvm.id.stripSuffix("JVM"))).settings(stdSettings(p.jvm.id.stripSuffix("JVM")))
  }

}
