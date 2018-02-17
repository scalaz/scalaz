import build._
import com.typesafe.sbt.osgi.OsgiKeys
import org.scalajs.sbtplugin.cross._
import sbtunidoc.Plugin.UnidocKeys._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val jsProjects = Seq[ProjectReference](
  coreJS, effectJS, iterateeJS, scalacheckBindingJS, testsJS
)

lazy val jvmProjects = Seq[ProjectReference](
  coreJVM, effectJVM, iterateeJVM, scalacheckBindingJVM, testsJVM, concurrent, example
)

lazy val nativeProjects = Seq[ProjectReference](
  coreNative, effectNative, iterateeNative, nativeTest
)

lazy val scalaz = Project(
  id = "scalaz",
  base = file("."),
  settings = standardSettings ++ unidocSettings ++ Seq[Sett](
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
    }
  ) ++ Defaults.packageTaskSettings(packageDoc in Compile, (unidoc in Compile).map(_.flatMap(Path.allSubpaths))),
  aggregate = jvmProjects ++ jsProjects
)

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
  base = file("concurrent"),
  settings = standardSettings ++ Seq(
    name := ConcurrentName,
    typeClasses := TypeClass.concurrent,
    osgiExport("scalaz.concurrent"),
    OsgiKeys.importPackage := Seq("javax.swing;resolution:=optional", "*")
  ),
  dependencies = Seq(coreJVM, effectJVM)
)

lazy val effectJVM = effect.jvm
lazy val effectJS  = effect.js
lazy val effectNative = effect.native

lazy val iterateeJVM = iteratee.jvm
lazy val iterateeJS  = iteratee.js
lazy val iterateeNative = iteratee.native

lazy val example = Project(
  id = "example",
  base = file("example"),
  dependencies = Seq(coreJVM, iterateeJVM, concurrent),
  settings = standardSettings ++ Seq[Sett](
    name := "scalaz-example",
    publishArtifact := false
  )
)

lazy val scalacheckBinding =
  crossProject(JVMPlatform, JSPlatform).crossType(ScalazCrossType)
    .in(file("scalacheck-binding"))
    .settings(standardSettings)
    .settings(
      name := "scalaz-scalacheck-binding",
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion.value,
      osgiExport("scalaz.scalacheck"))
    .dependsOn(core, iteratee)
    .jvmConfigure(_ dependsOn concurrent)
    .jsSettings(scalajsProjectSettings)

lazy val scalacheckBindingJVM = scalacheckBinding.jvm
lazy val scalacheckBindingJS  = scalacheckBinding.js

lazy val tests = crossProject(JSPlatform, JVMPlatform).crossType(ScalazCrossType)
  .settings(standardSettings)
  .settings(
    name := "scalaz-tests",
    publishArtifact := false,
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion.value % "test")
  .dependsOn(core, effect, iteratee, scalacheckBinding)
  .jvmConfigure(_ dependsOn concurrent)
  .jsSettings(scalajsProjectSettings)

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
