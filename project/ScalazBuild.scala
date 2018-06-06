import sbt._
import Keys._
import org.scalajs.sbtplugin.cross.CrossProject

object Scalaz {
  val testDeps        = Seq("org.scalacheck"  %% "scalacheck"   % "1.14.0" % "test")
  val compileOnlyDeps = Seq("com.github.ghik" %% "silencer-lib" % "1.0"    % "provided")

  private val stdOptions = Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-explaintypes",
    "-Yrangepos",
    "-feature",
    "-Xfuture",
    "-Ypartial-unification",
    "-language:higherKinds",
    "-language:existentials",
    "-unchecked",
    "-Yno-adapted-args",
    "-opt-warnings",
    "-Xlint:_,-type-parameter-shadow",
    "-Xsource:2.13",
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
    "-opt-inline-from:<source>",
    "-Yno-imports"
  )

  def stdSettings(prjName: String) = Seq(
    name := s"scalaz-$prjName",
    scalacOptions := stdOptions,
    scalacOptions in (Compile, compile) ++=
      Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
    libraryDependencies ++= compileOnlyDeps ++ testDeps ++ Seq(
      compilerPlugin("org.spire-math"         %% "kind-projector"  % "0.9.7"),
      compilerPlugin("com.github.tomasmikula" %% "pascal"          % "0.2.1"),
      compilerPlugin("com.github.ghik"        %% "silencer-plugin" % "1.0")
    ),
    incOptions ~= (_.withLogRecompileOnMacro(false))
  )

  implicit class ModuleHelper(p: Project) {
    def module: Project = p.in(file(p.id)).settings(stdSettings(p.id))
  }

  implicit class CrossProjectModuleHelper(p: CrossProject.Builder) {
    def module: CrossProject =
      p.in(file(p.jvm.id.stripSuffix("JVM"))).settings(stdSettings(p.jvm.id.stripSuffix("JVM")))
  }

  def partestDependency(version: String): ModuleID =
    (CrossVersion.partialVersion(version): @unchecked) match {
      case Some((2L, 12L)) =>
        "org.scala-lang.modules" %% "scala-partest" % "1.1.8"
      case Some((2L, 13L)) =>
        "org.scala-lang" % "scala-partest" % version
    }

  val partestFramework = List(
    fork in Test := true,
    javaOptions in Test += s"-Dpartest.root=${(sourceDirectory in Test in LocalProject("plugin")).value}",
    testFrameworks += new TestFramework("scala.tools.partest.sbt.Framework"),
    definedTests in Test +=
      new sbt.TestDefinition(
        "partest",
        new sbt.testing.AnnotatedFingerprint {
          def isModule       = true
          def annotationName = "partest"
        },
        true,
        Array()
      ),
    testOptions in Test += Tests.Argument(
      s"""-Dpartest.scalac_opts=-Xplugin:${(packageBin in Compile in LocalProject("plugin")).value} """.trim,
    ),
  )
}
