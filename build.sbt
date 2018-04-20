import build._

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts
import sbtcrossproject.CrossPlugin.autoImport.crossProject

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
  coreJS, effectJS, iterateeJS, scalacheckBindingJS_1_12, scalacheckBindingJS_1_13, testsJS
)

lazy val jvmProjects = Seq[ProjectReference](
  coreJVM, effectJVM, iterateeJVM, scalacheckBindingJVM_1_12, scalacheckBindingJVM_1_13, testsJVM, concurrent, example
)

lazy val nativeProjects = Seq[ProjectReference](
  coreNative, effectNative, iterateeNative, nativeTest
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
    (jsProjects ++ nativeProjects).foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a)) -- inProjects(scalacheckBindingJVM_1_12)
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
)

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
  notPublish
).dependsOn(
  coreJVM, iterateeJVM, concurrent
)

def scalacheckBindingProject(id: String, base: String, scalacheckVersion: SettingKey[String]) =
  sbtcrossproject.CrossProject(id, file(base))(JVMPlatform, JSPlatform)
    .crossType(ScalazCrossType)
    .settings(standardSettings)
    .settings(
      name := "scalaz-scalacheck-binding",
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
      }
    )
    .jsSettings(
      (unmanagedSourceDirectories in Compile) += {
        (baseDirectory in LocalRootProject).value / "scalacheck-binding/js/src/main/scala"
      }
    )

lazy val scalacheckBinding_1_12 =
  scalacheckBindingProject("scalacheck-binding_1_12", "scalacheck-binding_1_12", scalaCheckVersion_1_12)
lazy val scalacheckBindingJVM_1_12 = scalacheckBinding_1_12.jvm
lazy val scalacheckBindingJS_1_12  = scalacheckBinding_1_12.js


lazy val scalacheckBinding_1_13 = {
  def scalacheckBinding_1_13Version(base: String) = base + "-scalacheck-1.13"

  scalacheckBindingProject("scalacheck-binding_1_13", "scalacheck-binding_1_13", scalaCheckVersion_1_13).settings(
    version ~= { v =>
      val snapshotSuffix = "-SNAPSHOT"
      if(v.endsWith(snapshotSuffix)) {
        scalacheckBinding_1_13Version(v.dropRight(snapshotSuffix.length)) + snapshotSuffix
      } else {
        scalacheckBinding_1_13Version(v)
      }
    }
  )
  .jvmSettings(
    mimaPreviousArtifacts := {
      scalazMimaBasis.?.value.map { v =>
        organization.value % s"${name.value}_${scalaBinaryVersion.value}" % scalacheckBinding_1_13Version(v)
      }.toSet
    }
  )
  .jsSettings(
    mimaPreviousArtifacts := {
      scalazMimaBasis.?.value.map { v =>
        organization.value % s"${name.value}_sjs0.6_${scalaBinaryVersion.value}" % scalacheckBinding_1_13Version(v)
      }.toSet
    }
  )
}

lazy val scalacheckBindingJVM_1_13 = scalacheckBinding_1_13.jvm
lazy val scalacheckBindingJS_1_13  = scalacheckBinding_1_13.js


lazy val tests = crossProject(JSPlatform, JVMPlatform).crossType(ScalazCrossType)
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
  .jvmSettings(
    minSuccessfulTests := 33
  )
  .jsSettings(
    minSuccessfulTests := 10
  )
  .dependsOn(core, effect, iteratee, scalacheckBinding_1_13)
  .jvmConfigure(_ dependsOn concurrent)
  .jsSettings(scalajsProjectSettings)
  .settings(
    notPublish
  )

lazy val testsJVM = tests.jvm
lazy val testsJS  = tests.js

// can't use "sbt test"
// https://github.com/scala-native/scala-native/issues/339
lazy val nativeTest = Project(nativeTestId, file("nativeTest")).enablePlugins(ScalaNativePlugin)
  .settings(
    standardSettings,
    nativeSettings,
    notPublish
  )
  .dependsOn(iterateeNative)
