import build._
import com.typesafe.tools.mima.core.{DirectMissingMethodProblem, ProblemFilters}
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts

val scalaVersions = Seq(Scala212, Scala213, Scala3)

val minSuccessfulTests = settingKey[Int]("")

/*
 * NOTICE if you are a contributor who only cares about the JVM, create a file
 * `local.sbt` containing
 *
 *   onLoad in Global := { s => "project rootJVM" :: s }
 *
 * and regular commands such as "compile" / "test" will skip over all the
 * scalajs / scala-native stuff.
 */


lazy val all = Seq(core, effect, iteratee, scalacheckBinding, tests, example)

def rootScalaVersion = Scala3

lazy val scalaz = Project(
  id = "scalaz",
  base = file(".")
).settings(
  standardSettings,
  scalaVersion := rootScalaVersion,
  mimaPreviousArtifacts := Set.empty,
  description := "scalaz unidoc",
  artifacts := Classpaths.artifactDefs(Seq(Compile / packageDoc, Compile / makePom)).value,
  packagedArtifacts := Classpaths.packaged(Seq(Compile / packageDoc, Compile / makePom)).value,
  pomPostProcess := { node =>
    import scala.xml._
    import scala.xml.transform._
    val rule = new RewriteRule {
      override def transform(n: Node) =
        if (n.label == "dependencies") NodeSeq.Empty else n
    }
    new RuleTransformer(rule).transform(node)(0)
  },
  ScalaUnidoc / unidoc / unidocProjectFilter := {
    inProjects(all.map(_.jvm(rootScalaVersion): ProjectReference)*)
  },
  Defaults.packageTaskSettings((Compile / packageDoc), (Compile / unidoc).map(_.flatMap(Path.allSubpaths)))
).aggregate(
  all.flatMap(_.allProjects().map(_._1: ProjectReference))*
).enablePlugins(ScalaUnidocPlugin)

lazy val rootNative = Project(
  rootNativeId,
  file("rootNative")
).settings(
  standardSettings,
  autoScalaLibrary := false,
  notPublish
).aggregate(all.flatMap(_.native.get).map(p => p: ProjectReference)*)

lazy val rootJS = Project(
  "rootJS",
  file("rootJS")
).settings(
  standardSettings,
  autoScalaLibrary := false,
  notPublish
).aggregate(all.flatMap(_.js.get).map(p => p: ProjectReference)*)

lazy val rootJVM = Project(
  "rootJVM",
  file("rootJVM")
).settings(
  standardSettings,
  autoScalaLibrary := false,
  notPublish
).aggregate(all.flatMap(_.jvm.get).map(p => p: ProjectReference)*)

lazy val core = projectMatrix
  .defaultAxes()
  .settings(
    standardSettings,
    name := "scalaz-core",
    (Compile / sourceGenerators) += (Compile / sourceManaged).map{
      dir => Seq(GenerateTupleW(dir), TupleNInstances(dir))
    }.taskValue,
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
    buildInfoPackage := buildInfoPackageName,
    buildInfoObject := "ScalazBuildInfo",
  )
  .enablePlugins(sbtbuildinfo.BuildInfoPlugin, MimaPlugin)
  .jsPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jsSettings,
    ),
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      jvm_js_settings,
      mimaBinaryIssueFilters ++= Seq(
        ProblemFilters.exclude[DirectMissingMethodProblem]("scalaz.std.AllInstances.<clinit>"),
      ),
      typeClasses := TypeClass.core
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
    ),
  )

lazy val effect = projectMatrix
  .defaultAxes()
  .settings(
    standardSettings,
    name := "scalaz-effect",
  )
  .dependsOn(core)
  .enablePlugins(MimaPlugin)
  .jsPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jsSettings,
    ),
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      jvm_js_settings,
      typeClasses := TypeClass.effect,
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
    ),
  )

lazy val iteratee = projectMatrix
  .defaultAxes()
  .settings(
    standardSettings,
    name := "scalaz-iteratee",
  )
  .dependsOn(core, effect)
  .enablePlugins(MimaPlugin)
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      jvm_js_settings,
    )
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jsSettings,
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
    ),
  )

lazy val example = projectMatrix
  .defaultAxes()
  .in(file("example"))
  .settings(
    standardSettings,
    name := "scalaz-example",
    Compile / compile / scalacOptions -= "-Xlint:adapted-args",
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      jvm_js_settings,
      TaskKey[Unit]("runAllMain") := {
        val r = (run / runner).value
        val classpath = (Compile / fullClasspath).value
        val log = streams.value.log
        (Compile / discoveredMainClasses).value.sorted.foreach(c =>
          r.run(c, classpath.map(_.data), Nil, log)
        )
      },
      notPublish,
    )
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      scalaJSUseMainModuleInitializer := true,
      commands += Command.command("runAllMain") { state1 =>
        val extracted = Project.extract(state1)
        val (state2, classes) = extracted.runTask(Compile / discoveredMainClasses, state1)
        classes.sorted.flatMap(c => s"""set Compile / mainClass := Some("$c")""" :: "run" :: Nil).toList ::: state2
      },
      notPublish,
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
      commands += Command.command("runAllMain") { state1 =>
        val extracted = Project.extract(state1)
        val (state2, classes) = extracted.runTask(Compile / discoveredMainClasses, state1)
        classes.sorted.flatMap(c => s"""set Compile / selectMainClass := Some("$c")""" :: "run" :: Nil).toList ::: state2
      },
      notPublish,
    ),
  )
  .dependsOn(
    iteratee
  )

lazy val scalacheckBinding = projectMatrix
  .defaultAxes()
  .in(file("scalacheck-binding"))
  .enablePlugins(MimaPlugin)
  .settings(standardSettings)
  .settings(
    name := "scalaz-scalacheck-binding",
    Compile / compile / scalacOptions -= "-Ywarn-value-discard",
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.19.0",
  )
  .dependsOn(core, iteratee)
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      jvm_js_settings,
    )
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jsSettings
    )
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
      evictionErrorLevel := Level.Warn,
    ),
  )

lazy val tests = projectMatrix
  .defaultAxes()
  .settings(standardSettings)
  .settings(
    name := "scalaz-tests",
    (Test / testOptions) += {
      val scalacheckOptions = Seq(
        "-maxSize", "5",
        "-workers", "1",
        "-maxDiscardRatio", "50",
        "-minSuccessfulTests", minSuccessfulTests.value.toString
      )
      Tests.Argument(TestFrameworks.ScalaCheck, scalacheckOptions*)
    },
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      jvm_js_settings,
      minSuccessfulTests := 33,
      notPublish,
    )
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
      minSuccessfulTests := 33,
      evictionErrorLevel := Level.Warn,
      notPublish,
    )
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jsSettings,
      minSuccessfulTests := 10,
      notPublish,
    )
  )
  .dependsOn(core, effect, iteratee, scalacheckBinding)
