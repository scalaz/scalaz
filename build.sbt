import GenTypeClass._

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi._

import sbtbuildinfo.Plugin._

import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts
import sbtunidoc.Plugin.UnidocKeys._

import build._

lazy val scalaz = Project(
  id = "scalaz",
  base = file("."),
  settings = standardSettings ++ unidocSettings ++ Seq[Sett](
    mimaPreviousArtifacts := Set.empty,
    // <https://github.com/scalaz/scalaz/issues/261>
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(typelevel),
    artifacts <<= Classpaths.artifactDefs(Seq(packageDoc in Compile)),
    packagedArtifacts <<= Classpaths.packaged(Seq(packageDoc in Compile))
  ) ++ Defaults.packageTaskSettings(packageDoc in Compile, (unidoc in Compile).map(_.flatMap(Path.allSubpaths))),
  aggregate = Seq(core, concurrent, effect, example, iteratee, scalacheckBinding, tests, typelevel, xml)
)

lazy val core = Project(
  id = "core",
  base = file("core"),
  settings = standardSettings ++ buildInfoSettings ++ Seq[Sett](
    name := "scalaz-core",
    typeClasses := TypeClass.core,
    sourceGenerators in Compile <+= (sourceManaged in Compile) map {
      dir => Seq(GenerateTupleW(dir))
    },
    scalaParserCombinatorsVersion := "1.0.4",
    scalaXmlVersion := "1.0.5",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          Seq(
            "org.scala-lang.modules" %% "scala-parser-combinators" % scalaParserCombinatorsVersion.value,
            "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion.value
          )
        case _ =>
          Nil
      }
    },
    sourceGenerators in Compile <+= buildInfo,
    buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
    buildInfoPackage := "scalaz",
    osgiExport("scalaz"),
    OsgiKeys.importPackage := Seq("javax.swing;resolution:=optional", "*")
  )
)

lazy val concurrent = Project(
  id = "concurrent",
  base = file("concurrent"),
  settings = standardSettings ++ Seq[Sett](
    name := "scalaz-concurrent",
    typeClasses := TypeClass.concurrent,
    osgiExport("scalaz.concurrent"),
    OsgiKeys.importPackage := Seq("javax.swing;resolution:=optional", "*")
  ),
  dependencies = Seq(core, effect)
)

lazy val effect = Project(
  id = "effect",
  base = file("effect"),
  settings = standardSettings ++ Seq[Sett](
    name := "scalaz-effect",
    typeClasses := TypeClass.effect,
    osgiExport("scalaz.effect", "scalaz.std.effect", "scalaz.syntax.effect")
  ),
  dependencies = Seq(core)
)

lazy val iteratee = Project(
  id = "iteratee",
  base = file("iteratee"),
  settings = standardSettings ++ Seq[Sett](
    name := "scalaz-iteratee",
    osgiExport("scalaz.iteratee")
  ),
  dependencies = Seq(effect)
)

lazy val typelevel = Project(
  id = "typelevel",
  base = file("typelevel"),
  settings = standardSettings ++ Seq[Sett](
    name := "scalaz-typelevel",
    osgiExport("scalaz.typelevel", "scalaz.syntax.typelevel")
  ),
  dependencies = Seq(core)
)

lazy val xml = Project(
  id = "xml",
  base = file("xml"),
  settings = standardSettings ++ Seq[Sett](
    name := "scalaz-xml",
    typeClasses := TypeClass.xml,
    osgiExport("scalaz.xml")
  ),
  dependencies = Seq(core)
)

lazy val example = Project(
  id = "example",
  base = file("example"),
  dependencies = Seq(core, iteratee, concurrent, typelevel, xml),
  settings = standardSettings ++ Seq[Sett](
    name := "scalaz-example",
    mimaPreviousArtifacts := Set.empty,
    publishArtifact := false
  )
)

lazy val scalacheckBinding = Project(
  id           = "scalacheck-binding",
  base         = file("scalacheck-binding"),
  dependencies = Seq(core, concurrent, typelevel, xml, iteratee),
  settings     = standardSettings ++ Seq[Sett](
    name := "scalaz-scalacheck-binding",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % scalacheckVersion.value,
    osgiExport("scalaz.scalacheck")
  )
)

lazy val tests = Project(
  id = "tests",
  base = file("tests"),
  dependencies = Seq(core, iteratee, concurrent, effect, typelevel, xml, scalacheckBinding % "test"),
  settings = standardSettings ++Seq[Sett](
    name := "scalaz-tests",
    publishArtifact := false,
    mimaPreviousArtifacts := Set.empty,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % scalacheckVersion.value % "test"
  )
)
