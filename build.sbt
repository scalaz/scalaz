import build._

val scalaVersions = Seq(Scala213, Scala3)

val minSuccessfulTests = settingKey[Int]("")

/*
 * NOTICE if you are a contributor who only cares about the JVM, create a file
 * `local.sbt` containing
 *
 *   Global / onLoad := { s => "project rootJVM" :: s }
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
  description := "scalaz unidoc",
  artifacts := Classpaths.artifactDefs(Seq(Compile / packageDoc, Compile / makePom)).value,
  packagedArtifacts := Def.uncached(
    Classpaths.packaged(Seq(Compile / packageDoc, Compile / makePom)).value
  ),
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
  Defaults.packageTaskSettings(
    Compile / packageDoc,
    Def.task {
      given FileConverter = fileConverter.value
      (Compile / unidoc).value.flatMap(Mapper.allSubpaths)
    }
  ),
).aggregate(
  all.flatMap(_.allProjects().map(_._1: ProjectReference))*
).enablePlugins(ScalaUnidocPlugin)

lazy val rootNative = Project(
  "rootNative",
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
    unmanagedSourcePathSettings,
    name := "scalaz-core",
    Compile / sourceGenerators += (Compile / sourceManaged).map{
      dir => Seq(GenerateTupleW(dir), TupleNInstances(dir))
    }.taskValue,
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
    buildInfoPackage := build.buildInfoPackageName,
    buildInfoObject := "ScalazBuildInfo",
  )
  .enablePlugins(sbtbuildinfo.BuildInfoPlugin)
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings,
      jsSettings,
      jvm_js_settings,
      libraryDependencies += ("org.scala-js" %% "scalajs-weakreferences" % "1.0.0" % Optional).cross(CrossVersion.for3Use2_13)
    )
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      jvm_js_settings,
      typeClasses := TypeClass.core
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
    )
  )

lazy val effect = projectMatrix
  .defaultAxes()
  .settings(
    standardSettings,
    unmanagedSourcePathSettings,
    name := "scalaz-effect",
  )
  .dependsOn(core)
  .jsPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jsSettings,
      scalajsProjectSettings
    )
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jvmSettings,
      typeClasses := TypeClass.effect
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
    )
  )

lazy val iteratee = projectMatrix
  .defaultAxes()
  .settings(
    standardSettings,
    unmanagedSourcePathSettings,
    name := "scalaz-iteratee",
  )
  .dependsOn(core, effect)
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jvmSettings,
    ),
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings,
      jsSettings,
      jvm_js_settings,
    )
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
    )
  )

lazy val example = projectMatrix
  .defaultAxes()
  .in(file("example"))
  .settings(
    standardSettings,
    unmanagedSourcePathSettings,
    name := "scalaz-example",
    notPublish,
    Compile / compile / scalacOptions -= "-Xlint:adapted-args",
    Test / sourceGenerators += Def.task {
      val dir = (Test / sourceManaged).value
      val values = (Compile / discoveredMainClasses).value.sorted
      assert(values.nonEmpty, "discoveredMainClasses is empty")
      val properties = values.map { t =>
        s"""  property("${t}") = Prop { ${t}.main(Array.empty[String]) ; true } """
      }.mkString("\n")

      val src = s"""package scalaz
        |
        |import org.scalacheck.Prop
        |
        |object ExampleSpec extends org.scalacheck.Properties("ExampleSpec") {
        |
        |$properties
        |
        |}""".stripMargin

      val f = dir / "ExampleSpec.scala"
      IO.write(f, src)
      Seq(f)
    }.taskValue,
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jvmSettings,
      TaskKey[Unit]("runAllMain") := Def.uncached {
        val r = (run / runner).value
        val classpath = (Compile / fullClasspath).value.map(_.data).map(fileConverter.value.toPath)
        val log = streams.value.log
        (Compile / discoveredMainClasses).value.sorted.foreach(c =>
          r.run(c, classpath, Nil, log)
        )
      },
    )
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings,
      jsSettings,
      jvm_js_settings,
      scalaJSUseMainModuleInitializer := true,
      commands += Command.command("runAllMain") { state1 =>
        val extracted = Project.extract(state1)
        val (state2, classes) = extracted.runTask(Compile / discoveredMainClasses, state1)
        classes.sorted.flatMap(c => s"""set Compile / mainClass := Some("$c")""" :: "run" :: Nil).toList ::: state2
      },
    )
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
      evictionErrorLevel := Level.Warn,
      commands += Command.command("runAllMain") { state1 =>
        val extracted = Project.extract(state1)
        val (state2, classes) = extracted.runTask(Compile / discoveredMainClasses, state1)
        classes.sorted.flatMap(c => s"""set Compile / selectMainClass := Some("$c")""" :: "run" :: Nil).toList ::: state2
      },
    )
  ).dependsOn(
    core, iteratee, scalacheckBinding
  )

lazy val scalacheckBinding = projectMatrix
  .defaultAxes()
  .in(file("scalacheck-binding"))
  .settings(standardSettings)
  .settings(
    unmanagedSourcePathSettings,
    name := "scalaz-scalacheck-binding",
    Compile / compile / scalacOptions -= "-Ywarn-value-discard",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.19.0",
  )
  .dependsOn(core, iteratee)
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jvmSettings,
    ),
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jsSettings,
      scalajsProjectSettings
    )
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
      evictionErrorLevel := Level.Warn,
    )
  )

lazy val tests = projectMatrix
  .defaultAxes()
  .settings(standardSettings)
  .settings(
    unmanagedSourcePathSettings,
    name := "scalaz-tests",
    notPublish,
    (Test / testOptions) += {
      val scalacheckOptions = Seq(
        "-maxSize", "5",
        "-workers", "1",
        "-maxDiscardRatio", "50",
        "-minSuccessfulTests", minSuccessfulTests.value.toString
      )
      Tests.Argument(TestFrameworks.ScalaCheck, scalacheckOptions*)
    },
    (Test / sources) := {
      val exclude = Set(
        "MonadErrorTest.scala",
        "UnapplyTest.scala",
      )
      val list = (Test / sources).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          list.filterNot { src =>
            exclude.contains(src.getName)
          }
        case _ =>
          list
      }
    },
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvm_js_settings,
      jvmSettings,
      minSuccessfulTests := 33,
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
      evictionErrorLevel := Level.Warn,
      minSuccessfulTests := 33,
    )
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings,
      jsSettings,
      jvm_js_settings,
      minSuccessfulTests := 10,
      libraryDependencies += ("org.scala-js" %% "scalajs-weakreferences" % "1.0.0" % Test).cross(CrossVersion.for3Use2_13)
    )
  )
  .dependsOn(core, effect, iteratee, scalacheckBinding)
