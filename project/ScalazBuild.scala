import sbt._
import Keys._
import org.scalajs.sbtplugin.cross.CrossProject

object Scalaz {
  val silencerPlugin = compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.2")

  val testDeps        = Seq("org.scalacheck"  %% "scalacheck"   % "1.14.0" % "test")
  val compileOnlyDeps = Seq("com.github.ghik" %% "silencer-lib" % "1.2"    % "provided")

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
    "-Yno-imports",
    "-Yno-predef"
  )

  def stdSettings(prjName: String) =
    Seq(
      name := s"scalaz-$prjName",
      scalacOptions := stdOptions,
      scalacOptions in (Compile, compile) ++=
        Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
      libraryDependencies ++= compileOnlyDeps ++ testDeps ++ Seq(
        compilerPlugin("org.spire-math"         %% "kind-projector" % "0.9.7"),
        compilerPlugin("com.github.tomasmikula" %% "pascal"         % "0.3"),
        silencerPlugin,
        compilerPlugin("org.scalaz" %% "scalaz-plugin" % "0.0.7" cross CrossVersion.full),
        "org.scalaz" %% "scalaz-plugin-library" % "0.0.7"
      ),
      incOptions ~= (_.withLogRecompileOnMacro(false))
    ) ++ {
      Seq(packageBin, packageDoc, packageSrc).flatMap {
        // include LICENSE.txt in all packaged artifacts
        inTask(_)(Seq(mappings in Compile += licenseFile.value -> "LICENSE"))
      }
    }

  val licenseFile = settingKey[File]("The license file to include in packaged artifacts")
  val findLicense = licenseFile in Global := {
    val LICENSE_txt = (baseDirectory in ThisBuild).value / "LICENSE.txt"
    if (!LICENSE_txt.exists()) sys.error(s"cannot find license file at $LICENSE_txt")
    LICENSE_txt
  }

  implicit class ModuleHelper(p: Project) {
    def module: Project = p.in(file(p.id)).settings(stdSettings(p.id))
  }

  implicit class CrossProjectModuleHelper(p: CrossProject.Builder) {
    def module: CrossProject =
      p.in(file(p.jvm.id.stripSuffix("JVM"))).settings(stdSettings(p.jvm.id.stripSuffix("JVM")))
  }

}
