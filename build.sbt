import build._
import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts

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

lazy val jsProjects = Seq[ProjectReference](
  coreJS, effectJS, iterateeJS, scalacheckBindingJS, testsJS, exampleJS
)

lazy val jvmProjects = Seq[ProjectReference](
  coreJVM, effectJVM, iterateeJVM, scalacheckBindingJVM, testsJVM, exampleJVM
)

lazy val nativeProjects = Seq[ProjectReference](
  coreNative, effectNative, iterateeNative, scalacheckBindingNative, testsNative, exampleNative
)

lazy val scalaz = Project(
  id = "scalaz",
  base = file(".")
).settings(
  standardSettings,
  mimaPreviousArtifacts := Set.empty,
  description := "scalaz unidoc",
  artifacts := Classpaths.artifactDefs(Seq(Compile / packageDoc, Compile / makePom)).value,
  packagedArtifacts := Classpaths.packaged(Seq(Compile / packageDoc, Compile / makePom)).value,
  ScalaUnidoc / unidoc / unidocAllSources := {
    scalaBinaryVersion.value match {
      case "3" =>
        // TODO OutOfMemoryError
        Nil
      case _ =>
        (ScalaUnidoc / unidoc / unidocAllSources).value
    }
  },
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
    (jsProjects ++ nativeProjects).foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a))
  },
  Defaults.packageTaskSettings((Compile / packageDoc), (Compile / unidoc).map(_.flatMap(Path.allSubpaths)))
).aggregate(
  jvmProjects ++ jsProjects : _*
).enablePlugins(ScalaUnidocPlugin)

lazy val rootNative = Project(
  rootNativeId,
  file("rootNative")
).settings(
  standardSettings,
  notPublish
).aggregate(nativeProjects: _*)

lazy val rootJS = Project(
  "rootJS",
  file("rootJS")
).settings(
  standardSettings,
  notPublish
).aggregate(jsProjects: _*)

lazy val rootJVM = Project(
  "rootJVM",
  file("rootJVM")
).settings(
  standardSettings,
  notPublish
).aggregate(jvmProjects: _*)

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js
lazy val coreNative = core.native

lazy val effectJVM = effect.jvm
lazy val effectJS  = effect.js
lazy val effectNative = effect.native

lazy val iterateeJVM = iteratee.jvm
lazy val iterateeJS  = iteratee.js
lazy val iterateeNative = iteratee.native

lazy val exampleJVM = example.jvm
lazy val exampleJS = example.js
lazy val exampleNative = example.native

lazy val example = crossProject(JVMPlatform, JSPlatform, NativePlatform)
  .crossType(ScalazCrossType)
  .in(file("example"))
  .settings(
    standardSettings,
    name := "scalaz-example",
    notPublish,
    Compile / compile / scalacOptions -= "-Xlint:adapted-args",
  )
  .jvmSettings(
    TaskKey[Unit]("runAllMain") := {
      val r = (run / runner).value
      val classpath = (Compile / fullClasspath).value
      val log = streams.value.log
      (Compile / discoveredMainClasses).value.sorted.foreach(c =>
        r.run(c, classpath.map(_.data), Nil, log)
      )
    },
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    commands += Command.command("runAllMain") { state1 =>
      val extracted = Project.extract(state1)
      val (state2, classes) = extracted.runTask(Compile / discoveredMainClasses, state1)
      classes.sorted.flatMap(c => s"""set Compile / mainClass := Some("$c")""" :: "run" :: Nil).toList ::: state2
    },
  )
  .nativeSettings(
    nativeSettings,
    commands += Command.command("runAllMain") { state1 =>
      val extracted = Project.extract(state1)
      val (state2, classes) = extracted.runTask(Compile / discoveredMainClasses, state1)
      classes.sorted.flatMap(c => s"""set Compile / selectMainClass := Some("$c")""" :: "run" :: Nil).toList ::: state2
    },
  )
  .settings(
    mimaPreviousArtifacts := Set.empty,
  )
  .dependsOn(
    iteratee
  )

lazy val scalacheckBinding =
  crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(ScalazCrossType)
    .in(file("scalacheck-binding"))
    .enablePlugins(MimaPlugin)
    .settings(standardSettings)
    .settings(
      name := "scalaz-scalacheck-binding",
      Compile / compile / scalacOptions -= "-Ywarn-value-discard",
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.18.0",
      osgiExport("scalaz.scalacheck")
    )
    .dependsOn(core, iteratee)
    .jsSettings(scalajsProjectSettings)
    .nativeSettings(nativeSettings)

lazy val scalacheckBindingJVM = scalacheckBinding.jvm
lazy val scalacheckBindingJS  = scalacheckBinding.js
lazy val scalacheckBindingNative = scalacheckBinding.native

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(ScalazCrossType)
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
      Tests.Argument(TestFrameworks.ScalaCheck, scalacheckOptions: _*)
    },
  )
  .nativeSettings(
    nativeSettings,
  )
  .platformsSettings(JVMPlatform, NativePlatform)(
    minSuccessfulTests := 33
  )
  .jsSettings(
    minSuccessfulTests := 10
  )
  .dependsOn(core, effect, iteratee, scalacheckBinding)
  .jsSettings(scalajsProjectSettings)
  .settings(
    notPublish
  )

lazy val testsJVM = tests.jvm
lazy val testsJS  = tests.js
lazy val testsNative = tests.native
