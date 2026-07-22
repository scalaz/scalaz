import build._

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


lazy val all = Seq(core, effect, iteratee, scalacheckBinding_1_15, tests, example)

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
  packagedArtifacts := Def.uncached(Classpaths.packaged(Seq(Compile / packageDoc, Compile / makePom)).value),
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
  .settings(standardSettings)
  .settings(
    mimaBinaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      Nil
    },
    name := "scalaz-core",
    (Compile / sourceGenerators) += (Compile / sourceManaged).map{
      dir => Seq(GenerateTupleW(dir), TupleNInstances(dir))
    }.taskValue,
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
    buildInfoPackage := buildInfoPackageName,
  )
  .enablePlugins(sbtbuildinfo.BuildInfoPlugin, MimaPlugin)
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      typeClasses := TypeClass.core
    ),
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings
    ),
  )

lazy val dependsOnConcurrent: Project => Project = { p =>
  if (p.id.contains("JVM")) {
    p.dependsOn(concurrent.jvm(
      p.id.split("JVM").last match {
        case "2_12" =>
          Scala212
        case "2_13" =>
          Scala213
        case "3" =>
          Scala3
      }
    ))
  } else {
    p
  }
}

lazy val concurrent = projectMatrix
  .defaultAxes()
  .settings(
    standardSettings,
    name := ConcurrentName,
    typeClasses := TypeClass.concurrent,
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
    ),
  )
  .dependsOn(
    core, effect
  )
  .enablePlugins(MimaPlugin)

lazy val effect = projectMatrix
  .defaultAxes()
  .settings(standardSettings)
  .settings(
    name := "scalaz-effect",
  )
  .dependsOn(core)
  .enablePlugins(MimaPlugin)
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      typeClasses := TypeClass.effect
    ),
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings
    ),
  )

lazy val iteratee = projectMatrix
  .defaultAxes()
  .settings(standardSettings)
  .settings(
    name := "scalaz-iteratee",
  )
  .dependsOn(core, effect)
  .enablePlugins(MimaPlugin)
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
    ),
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings,
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings
    ),
  )

lazy val example = projectMatrix
  .defaultAxes()
  .settings(
    standardSettings,
    name := "scalaz-example",
    notPublish
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      mimaPreviousArtifacts := Set.empty,
      TaskKey[Unit]("runAllMain") := Def.uncached {
        val r = (run / runner).value
        val classpath = (Compile / fullClasspath).value.map(_.data).map(fileConverter.value.toPath)
        val log = streams.value.log
        (Compile / discoveredMainClasses).value.sorted.foreach(c =>
          r.run(c, classpath, Nil, log)
        )
      },
    ),
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings,
      mimaPreviousArtifacts := Set.empty,
      scalaJSUseMainModuleInitializer := true,
      commands += Command.command("runAllMain") { state1 =>
        val extracted = Project.extract(state1)
        val (state2, classes) = extracted.runTask(Compile / discoveredMainClasses, state1)
        classes.sorted.flatMap(c => s"""set Compile / mainClass := Some("$c")""" :: "run" :: Nil).toList ::: state2
      },
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
      mimaPreviousArtifacts := Set.empty,
      commands += Command.command("runAllMain") { state1 =>
        val extracted = Project.extract(state1)
        val (state2, classes) = extracted.runTask(Compile / discoveredMainClasses, state1)
        classes.sorted.flatMap(c => s"""set Compile / selectMainClass := Some("$c")""" :: "run" :: Nil).toList ::: state2
      },
    ),
  )
  .configure(dependsOnConcurrent)
  .dependsOn(
    iteratee
  )

def scalacheckBindingProject(
  id: String,
  base: String,
  scalacheckVersion: String,
  versionSuffix: String,
) = {

  def fullVersion(base: String) = base + "-scalacheck-" + versionSuffix

  ProjectMatrix(id, file(base), this.getClass.getClassLoader)
    .defaultAxes()
    .settings(standardSettings)
    .settings(
      name := "scalaz-scalacheck-binding",
      version ~= { v =>
        val snapshotSuffix = "-SNAPSHOT"
        if(v.endsWith(snapshotSuffix)) {
          fullVersion(v.dropRight(snapshotSuffix.length)) + snapshotSuffix
        } else {
          fullVersion(v)
        }
      },
      (Compile / unmanagedSourceDirectories) += {
        (LocalRootProject / baseDirectory).value / "scalacheck-binding/src/main/scala"
      },
      libraryDependencies += "org.scalacheck" %% "scalacheck" % scalacheckVersion,
    )
    .configure(dependsOnConcurrent)
    .dependsOn(core, iteratee)
    .jvmPlatform(
      scalaVersions,
      Def.settings(
        jvmSettings,
        (Compile / unmanagedSourceDirectories) += {
          (LocalRootProject / baseDirectory).value / "scalacheck-binding/jvm/src/main/scala"
        },
        mimaPreviousArtifacts := {
          scalazMimaBasis.?.value.map { v =>
            organization.value % s"${name.value}_${scalaBinaryVersion.value}" % fullVersion(v)
          }.toSet
        }
      ),
    )
    .jsPlatform(
      scalaVersions,
      Def.settings(
        scalajsProjectSettings,
        (Compile / unmanagedSourceDirectories) += {
          (LocalRootProject / baseDirectory).value / "scalacheck-binding/js/src/main/scala"
        },
        mimaPreviousArtifacts := {
          scalazMimaBasis.?.value.map { v =>
            organization.value % s"${name.value}_sjs1_${scalaBinaryVersion.value}" % fullVersion(v)
          }.toSet
        }
      ),
    )
    .nativePlatform(
      scalaVersions,
      Def.settings(
        nativeSettings,
        evictionErrorLevel := Level.Warn,
        (Compile / unmanagedSourceDirectories) += {
          (LocalRootProject / baseDirectory).value / "scalacheck-binding/native/src/main/scala"
        },
        mimaPreviousArtifacts := {
          if (scalazMimaBasis.?.value == Some("7.2.34")) {
            Set.empty
          } else {
            scalazMimaBasis.?.value.map { v =>
              organization.value % s"${name.value}_native0.4_${scalaBinaryVersion.value}" % fullVersion(v)
            }.toSet
          }
        },
      ),
    )
}

lazy val scalacheckBinding_1_15 = scalacheckBindingProject(
  id = "scalacheck-binding_1_15",
  base = "scalacheck-binding_1_15",
  scalacheckVersion = "1.19.0",
  versionSuffix = "1.15",
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
    }
  )
  .jvmPlatform(
    scalaVersions,
    Def.settings(
      jvmSettings,
      mimaPreviousArtifacts := Set.empty,
      minSuccessfulTests := 33,
    ),
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings,
      mimaPreviousArtifacts := Set.empty,
      minSuccessfulTests := 10,
    ),
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      nativeSettings,
      mimaPreviousArtifacts := Set.empty,
      minSuccessfulTests := 33,
      evictionErrorLevel := Level.Warn,
    ),
  )
  .enablePlugins(MimaPlugin)
  .configure(dependsOnConcurrent)
  .dependsOn(core, effect, iteratee, scalacheckBinding_1_15)
  .settings(
    notPublish
  )
