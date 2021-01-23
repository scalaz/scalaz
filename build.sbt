import build._

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.Platform

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
  coreJS, effectJS, iterateeJS, scalacheckBindingJS_1_15, testsJS
)

lazy val jvmProjects = Seq[ProjectReference](
  coreJVM, effectJVM, iterateeJVM, scalacheckBindingJVM_1_15, testsJVM, concurrent, example
)

lazy val nativeProjects = Seq[ProjectReference](
  coreNative, effectNative, iterateeNative, scalacheckBindingNative_1_15, testsNative
)

lazy val scalaz = Project(
  id = "scalaz",
  base = file(".")
).settings(
  standardSettings,
  mimaPreviousArtifacts := Set.empty,
  description := "scalaz unidoc",
  artifacts := Classpaths.artifactDefs(Seq(packageDoc in Compile, makePom in Compile)).value,
  packagedArtifacts := Classpaths.packaged(Seq(packageDoc in Compile, makePom in Compile)).value,
  pomPostProcess := { node =>
    import scala.xml._
    import scala.xml.transform._
    val rule = new RewriteRule {
      override def transform(n: Node) =
        if (n.label == "dependencies") NodeSeq.Empty else n
    }
    new RuleTransformer(rule).transform(node)(0)
  },
  unidocProjectFilter in (ScalaUnidoc, unidoc) := {
    (jsProjects ++ nativeProjects).foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a))
  },
  Defaults.packageTaskSettings(packageDoc in Compile, (unidoc in Compile).map(_.flatMap(Path.allSubpaths)))
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

lazy val concurrent = Project(
  id = "concurrent",
  base = file("concurrent")
).settings(
  standardSettings,
  name := ConcurrentName,
  typeClasses := TypeClass.concurrent,
  osgiExport("scalaz.concurrent"),
  OsgiKeys.importPackage := Seq("javax.swing;resolution:=optional", "*")
).dependsOn(
  coreJVM, effectJVM
).enablePlugins(MimaPlugin)

lazy val effectJVM = effect.jvm
lazy val effectJS  = effect.js
lazy val effectNative = effect.native

lazy val iterateeJVM = iteratee.jvm
lazy val iterateeJS  = iteratee.js
lazy val iterateeNative = iteratee.native

lazy val example = Project(
  id = "example",
  base = file("example")
).settings(
  standardSettings,
  name := "scalaz-example",
  notPublish,
  TaskKey[Unit]("runAllMain") := {
    val r = (runner in run).value
    val classpath = (Compile / fullClasspath).value
    val log = streams.value.log
    (Compile / discoveredMainClasses).value.sorted.foreach(c =>
      r.run(c, classpath.map(_.data), Nil, log)
    )
  },
).dependsOn(
  coreJVM, iterateeJVM, concurrent
)

def scalacheckBindingProject(
  id: String,
  base: String,
  scalacheckVersion: SettingKey[String],
  versionSuffix: String,
  platforms: Seq[Platform]
) = {

  def fullVersion(base: String) = base + "-scalacheck-" + versionSuffix

  sbtcrossproject.CrossProject(id, file(base))(platforms: _*)
    .crossType(ScalazCrossType)
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
      (unmanagedSourceDirectories in Compile) += {
        (baseDirectory in LocalRootProject).value / "scalacheck-binding/src/main/scala"
      },
      libraryDependencies += scalaCheckGroupId.value %%% "scalacheck" % scalacheckVersion.value,
      osgiExport("scalaz.scalacheck"))
    .dependsOn(core, iteratee)
    .jvmConfigure(_ dependsOn concurrent)
    .jsSettings(scalajsProjectSettings)
    .jvmSettings(
      (unmanagedSourceDirectories in Compile) += {
        (baseDirectory in LocalRootProject).value / "scalacheck-binding/jvm/src/main/scala"
      },
      mimaPreviousArtifacts := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, v)) if v <= 13 =>
            scalazMimaBasis.?.value.map { v =>
              organization.value % s"${name.value}_${scalaBinaryVersion.value}" % fullVersion(v)
            }.toSet
          case _ =>
            Set.empty
        }

        // TODO enable if "-scalacheck-1.15" version released
        Set.empty
      }
    )
    .jsSettings(
      (unmanagedSourceDirectories in Compile) += {
        (baseDirectory in LocalRootProject).value / "scalacheck-binding/js/src/main/scala"
      },
      mimaPreviousArtifacts := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, v)) if v <= 13 =>
            scalazMimaBasis.?.value.map { v =>
              organization.value % s"${name.value}_sjs1_${scalaBinaryVersion.value}" % fullVersion(v)
            }.toSet
          case _ =>
            Set.empty
        }

        // TODO enable if "-scalacheck-1.15" version released
        Set.empty
      }
    )
    .nativeSettings(
      nativeSettings,
      (unmanagedSourceDirectories in Compile) += {
        (baseDirectory in LocalRootProject).value / "scalacheck-binding/native/src/main/scala"
      },
      mimaPreviousArtifacts := {
        // TODO enable if "-scalacheck-1.15" version released
        Set.empty
      },
    )
}

lazy val scalacheckBinding_1_15 = scalacheckBindingProject(
  id = "scalacheck-binding_1_15",
  base = "scalacheck-binding_1_15",
  scalacheckVersion = scalaCheckVersion_1_15,
  versionSuffix = "1.15",
  platforms = Seq(JVMPlatform, JSPlatform, NativePlatform)
)

lazy val scalacheckBindingJVM_1_15 = scalacheckBinding_1_15.jvm
lazy val scalacheckBindingJS_1_15  = scalacheckBinding_1_15.js
lazy val scalacheckBindingNative_1_15 = scalacheckBinding_1_15.native

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(ScalazCrossType)
  .settings(standardSettings)
  .settings(
    name := "scalaz-tests",
    testOptions in Test += {
      val scalacheckOptions = Seq(
        "-maxSize", "5",
        "-workers", "1",
        "-maxDiscardRatio", "50",
        "-minSuccessfulTests", minSuccessfulTests.value.toString
      )
      Tests.Argument(TestFrameworks.ScalaCheck, scalacheckOptions: _*)
    }
  )
  .nativeSettings(
    nativeSettings,
    (Test / sources) := {
      // https://github.com/scala-native/scala-native/issues/2125
      val exclude = Set(
        "DisjunctionTest.scala",
      )
      (Test / sources).value.filterNot { src =>
         exclude.contains(src.getName)
      }
    }
  )
  .platformsSettings(JVMPlatform, NativePlatform)(
    minSuccessfulTests := 33
  )
  .jsSettings(
    minSuccessfulTests := 10
  )
  .enablePlugins(MimaPlugin)
  .dependsOn(core, effect, iteratee, scalacheckBinding_1_15)
  .jvmConfigure(_ dependsOn concurrent)
  .jsSettings(scalajsProjectSettings)
  .settings(
    notPublish
  )

lazy val testsJVM = tests.jvm
lazy val testsJS  = tests.js
lazy val testsNative = tests.native
