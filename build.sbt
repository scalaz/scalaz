import build._

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts
import org.scalajs.sbtplugin.cross._
import sbtunidoc.Plugin.UnidocKeys._

lazy val jsProjects = Seq[ProjectReference](
  coreJS, effectJS, iterateeJS, scalacheckBindingJS, testsJS
)

lazy val jvmProjects = Seq[ProjectReference](
  coreJVM, effectJVM, iterateeJVM, scalacheckBindingJVM, testsJVM, concurrent, example
)

lazy val scalaz = Project(
  id = "scalaz",
  base = file("."),
  settings = standardSettings ++ unidocSettings ++ Seq[Sett](
    mimaPreviousArtifacts := Set.empty,
    artifacts <<= Classpaths.artifactDefs(Seq(packageDoc in Compile)),
    packagedArtifacts <<= Classpaths.packaged(Seq(packageDoc in Compile)),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := {
      jsProjects.foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a))
    }
  ) ++ Defaults.packageTaskSettings(packageDoc in Compile, (unidoc in Compile).map(_.flatMap(Path.allSubpaths))),
  aggregate = jvmProjects ++ jsProjects
)

lazy val rootJS = Project(
  "rootJS",
  file("rootJS")
).settings(
  standardSettings,
  notPublish,
  mimaPreviousArtifacts := Set.empty
).aggregate(jsProjects: _*)

lazy val rootJVM = Project(
  "rootJVM",
  file("rootJVM")
).settings(
  standardSettings,
  notPublish,
  mimaPreviousArtifacts := Set.empty
).aggregate(jvmProjects: _*)

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

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

lazy val iterateeJVM = iteratee.jvm
lazy val iterateeJS  = iteratee.js

lazy val example = Project(
  id = "example",
  base = file("example"),
  dependencies = Seq(coreJVM, iterateeJVM, concurrent),
  settings = standardSettings ++ Seq[Sett](
    name := "scalaz-example",
    mimaPreviousArtifacts := Set.empty,
    publishArtifact := false
  )
)

lazy val scalacheckBinding =
  CrossProject("scalacheck-binding", file("scalacheck-binding"), ScalazCrossType)
    .settings(standardSettings: _*)
    .settings(
      name := "scalaz-scalacheck-binding",
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion.value,
      osgiExport("scalaz.scalacheck"))
    .dependsOn(core, iteratee)
    .jvmConfigure(_ dependsOn concurrent)
    .jsSettings(scalajsProjectSettings : _*)

lazy val scalacheckBindingJVM = scalacheckBinding.jvm
lazy val scalacheckBindingJS  = scalacheckBinding.js

lazy val tests = crossProject.crossType(ScalazCrossType)
  .settings(standardSettings: _*)
  .settings(
    name := "scalaz-tests",
    mimaPreviousArtifacts := Set.empty,
    publishArtifact := false,
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion.value % "test")
  .dependsOn(core, effect, iteratee, scalacheckBinding)
  .jvmConfigure(_ dependsOn concurrent)
  .jsSettings(scalajsProjectSettings : _*)
  .jsSettings(
    jsEnv := NodeJSEnv().value,
    scalaJSUseRhino in Global := false
  )

lazy val testsJVM = tests.jvm
lazy val testsJS  = tests.js
