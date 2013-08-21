import sbt._
import Project.Setting
import Keys._

import GenTypeClass._

import java.awt.Desktop

import scala.collection.immutable.IndexedSeq

import sbtrelease._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

import com.typesafe.sbt.pgp.PgpKeys._

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi._

import sbtbuildinfo.Plugin._

import sbtunidoc.Plugin._
import sbtunidoc.Plugin.UnidocKeys._

object build extends Build {
  type Sett = Def.Setting[_]

  lazy val publishSignedArtifacts = ReleaseStep(
    action = st => {
      val extracted = st.extract
      val ref = extracted.get(thisProjectRef)
      extracted.runAggregated(publishSigned in Global in ref, st)
    },
    check = st => {
      // getPublishTo fails if no publish repository is set up.
      val ex = st.extract
      val ref = ex.get(thisProjectRef)
      Classpaths.getPublishTo(ex.get(publishTo in Global in ref))
      st
    },
    enableCrossBuild = true
  )

  lazy val standardSettings: Seq[Sett] = Defaults.defaultSettings ++ sbtrelease.ReleasePlugin.releaseSettings ++ Seq[Sett](
    organization := "org.scalaz",

    scalaVersion := "2.10.2",
    crossScalaVersions := Seq("2.9.3", "2.10.2"),
    resolvers += Resolver.sonatypeRepo("releases"),

    scalacOptions <++= (scalaVersion) map { sv =>
      val versionDepOpts =
        if (sv startsWith "2.9")
          Seq("-Ydependent-method-types", "-deprecation")
        else
          // does not contain -deprecation (because of ClassManifest)
          // contains -language:postfixOps (because 1+ as a parameter to a higher-order function is treated as a postfix op)
          Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps")

      Seq("-unchecked") ++ versionDepOpts
    },

    scalacOptions in (Compile, doc) <++= (baseDirectory in LocalProject("scalaz")) map { bd =>
      Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", "https://github.com/scalaz/scalaz/tree/scalaz-seven€{FILE_PATH}.scala")
    },

    // retronym: I was seeing intermittent heap exhaustion in scalacheck based tests, so opting for determinism.
    parallelExecution in Test := false,
    testOptions in Test += Tests.Argument("sequential"),

    (unmanagedClasspath in Compile) += Attributed.blank(file("dummy")),

    genTypeClasses <<= (scalaSource in Compile, streams, typeClasses) map {
      (scalaSource, streams, typeClasses) =>
        typeClasses.flatMap {
          tc =>
            val typeClassSource0 = typeclassSource(tc)
            typeClassSource0.sources.map(_.createOrUpdate(scalaSource, streams.log))
        }
    },
    checkGenTypeClasses <<= genTypeClasses.map{ classes =>
      if(classes.exists(_._1 != FileStatus.NoChange))
        sys.error(classes.groupBy(_._1).filterKeys(_ != FileStatus.NoChange).mapValues(_.map(_._2)).toString)
    },
    typeClasses := Seq(),
    genToSyntax <<= typeClasses map {
      (tcs: Seq[TypeClass]) =>
      val objects = tcs.map(tc => "object %s extends To%sSyntax".format(Util.initLower(tc.name), tc.name)).mkString("\n")
      val all = "object all extends " + tcs.map(tc => "To%sSyntax".format(tc.name)).mkString(" with ")
      objects + "\n\n" + all
    },
    typeClassTree <<= typeClasses map {
      tcs => tcs.map(_.doc).mkString("\n")
    },

    showDoc in Compile <<= (doc in Compile, target in doc in Compile) map { (_, out) =>
      val index = out / "index.html"
      if (index.exists()) Desktop.getDesktop.open(out / "index.html")
    },

    credentialsSetting,
    publishSetting,
    publishArtifact in Test := false,

    // adapted from sbt-release defaults
    // (performs `publish-signed` instead of `publish`)
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishSignedArtifacts,
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),

    pomIncludeRepository := {
      x => false
    },
    pomExtra := (
      <url>http://scalaz.org</url>
        <licenses>
          <license>
            <name>BSD-style</name>
            <url>http://www.opensource.org/licenses/bsd-license.php</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>git@github.com:scalaz/scalaz.git</url>
          <connection>scm:git:git@github.com:scalaz/scalaz.git</connection>
        </scm>
        <developers>
          {
          Seq(
            ("runarorama", "Runar Bjarnason"),
            ("pchiusano", "Paul Chiusano"),
            ("tonymorris", "Tony Morris"),
            ("retronym", "Jason Zaugg"),
            ("ekmett", "Edward Kmett"),
            ("alexeyr", "Alexey Romanov"),
            ("copumpkin", "Daniel Peebles"),
            ("rwallace", "Richard Wallace"),
            ("nuttycom", "Kris Nuttycombe"),
            ("larsrh", "Lars Hupel")
          ).map {
            case (id, name) =>
              <developer>
                <id>{id}</id>
                <name>{name}</name>
                <url>http://github.com/{id}</url>
              </developer>
          }
        }
        </developers>
      )
  ) ++ osgiSettings ++ Seq[Sett](
    OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
  )

  lazy val scalaz = Project(
    id = "scalaz",
    base = file("."),
    settings = standardSettings ++ unidocSettings ++ Seq[Sett](
      // <https://github.com/scalaz/scalaz/issues/261>
      excludedProjects in unidoc in ScalaUnidoc += "typelevel",
      publishArtifact := false
    ),
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
      libraryDependencies <++= scalaVersion{ v =>
        if((v startsWith "2.9") || (v startsWith "2.10")) Seq()
        else Seq(
          "org.scala-lang" % "scala-parser-combinators" % v,
          "org.scala-lang" % "scala-xml" % v
        )
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
      publishArtifact := false
    )
  )

  lazy val scalacheckBinding = Project(
    id           = "scalacheck-binding",
    base         = file("scalacheck-binding"),
    dependencies = Seq(core, concurrent, typelevel, xml, iteratee),
    settings     = standardSettings ++ Seq[Sett](
      name := "scalaz-scalacheck-binding",
      libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0",
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
      libraryDependencies <++= (scalaVersion) { sv => Seq(
        "org.specs2" %% "specs2" % Dependencies.specs2(sv) % "test",
        "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
      ) }
    )
  )

  object Dependencies {
    def specs2(scalaVersion: String) =
      if (scalaVersion startsWith "2.9")
        "1.12.4.1"
      else
        "1.12.3"
  }

  lazy val publishSetting = publishTo <<= (version).apply{
    v =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }

  lazy val credentialsSetting = credentials += {
    Seq("build.publish.user", "build.publish.password").map(k => Option(System.getProperty(k))) match {
      case Seq(Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
      case _                           =>
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }
  }

  lazy val genTypeClasses = TaskKey[Seq[(FileStatus, File)]]("gen-type-classes")

  lazy val typeClasses = TaskKey[Seq[TypeClass]]("type-classes")

  lazy val genToSyntax = TaskKey[String]("gen-to-syntax")

  lazy val showDoc = TaskKey[Unit]("show-doc")

  lazy val typeClassTree = TaskKey[String]("type-class-tree", "Generates scaladoc formatted tree of type classes.")

  lazy val checkGenTypeClasses = TaskKey[Unit]("check-gen-type-classes")

  def osgiExport(packs: String*) = OsgiKeys.exportPackage := packs.map(_ + ".*;version=${Bundle-Version}")
}

// vim: expandtab:ts=2:sw=2
