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

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact
import com.typesafe.tools.mima.plugin.MimaKeys.binaryIssueFilters
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

  lazy val setMimaVersion: ReleaseStep = { st: State =>
    val extracted = Project.extract(st)

    val (releaseV, _) = st.get(versions).getOrElse(sys.error("impossible"))
    // TODO switch to `versionFile` key when updating sbt-release
    IO.write(new File("version.sbt"), "\n\nscalazMimaBasis in ThisBuild := \"%s\"" format releaseV, append = true)
    reapply(Seq(scalazMimaBasis in ThisBuild := releaseV), st)
  }

  val latestScala211PreRelease = "2.11.0-RC1"

  lazy val standardSettings: Seq[Sett] = Defaults.defaultSettings ++ sbtrelease.ReleasePlugin.releaseSettings ++ Seq[Sett](
    organization := "org.scalaz",

    scalaVersion := "2.9.2",
    crossScalaVersions := Seq("2.9.2", "2.9.3", "2.10.1", "2.11.0-SNAPSHOT"),
    resolvers ++= (if (scalaVersion.value.endsWith("-SNAPSHOT")) List(Opts.resolver.sonatypeSnapshots) else Nil),

    scalaBinaryVersion in update := (
      if (scalaVersion.value == "2.11.0-SNAPSHOT") latestScala211PreRelease else scalaBinaryVersion.value
    ),

    scalacOptions <++= (scalaVersion) map { sv =>
      val versionDepOpts =
        if (sv startsWith "2.9")
          Seq("-Ydependent-method-types", "-deprecation")
        else
          // does not contain -deprecation (because of ClassManifest)
          // contains -language:postfixOps (because 1+ as a parameter to a higher-order function is treated as a postfix op)
          Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps")

      Seq("-unchecked") ++ versionDepOpts ++ (if (sv startsWith "2.11") Seq("-Xsource:2.10") else Seq())
    },

    scalacOptions in (Compile, doc) <++= (baseDirectory in LocalProject("scalaz"), version) map { (bd, v) =>
      val tagOrBranch = if(v endsWith "SNAPSHOT") "series/7.0.x" else ("v" + v)
      Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", "https://github.com/scalaz/scalaz/tree/" + tagOrBranch + "€{FILE_PATH}.scala")
    },

    // retronym: I was seeing intermittent heap exhaustion in scalacheck based tests, so opting for determinism.
    parallelExecution in Test := false,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1"),

    (unmanagedClasspath in Compile) += Attributed.blank(file("dummy")),

    genTypeClasses <<= (scalaSource in Compile, streams, typeClasses) map {
      (scalaSource, streams, typeClasses) =>
        typeClasses.flatMap {
          tc =>
            val typeClassSource0 = typeclassSource(tc)
            typeClassSource0.sources.map(_.createOrUpdate(scalaSource, streams.log))
        }
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
      setMimaVersion,
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
  ) ++ mimaDefaultSettings ++ Seq[Sett](
    binaryIssueFilters ++= {
      import com.typesafe.tools.mima.core._
      import com.typesafe.tools.mima.core.ProblemFilters._
      Seq(
        "scalaz.Cord.isEmpty",
        "scalaz.Cord.nonEmpty",
        "scalaz.EitherT.withFilter",
        "scalaz.EphemeralStream.inits",
        "scalaz.EphemeralStream.take",
        "scalaz.EphemeralStream.tails",
        "scalaz.EphemeralStream.takeWhile",
        "scalaz.LensFamily.modo",
        "scalaz.LensFamily.<%=",
        "scalaz.LensFamily.assigno",
        "scalaz.LensFamily.<:=",
        "scalaz.Validation.excepting",
        "scalaz.NonEmptyList.init",
        "scalaz.NonEmptyList.last",
        "scalaz.NonEmptyList.foreach",
        "scalaz.NonEmptyList.sortBy",
        "scalaz.NonEmptyList.sortWith",
        "scalaz.NonEmptyList.sorted",
        "scalaz.concurrent.Future.timed",
        "scalaz.concurrent.Node.a_=",
        "scalaz.syntax.typelevel.HLists.::",
        "scalaz.syntax.typelevel.HLists.scalaz$syntax$typelevel$HLists$_setter_$::_=",
        "scalaz.concurrent.Actor.scalaz$concurrent$Actor$$act"
      ).map(exclude[MissingMethodProblem]) ++
      Seq(
        "scalaz.concurrent.Node$"
      ).map(exclude[MissingClassProblem])
    }
  ) ++ Seq[Sett](
    previousArtifact <<= (organization, name, scalaBinaryVersion, scalazMimaBasis.?) { (o, n, sbv, basOpt) =>
      basOpt match {
        case Some(bas) if !(sbv startsWith "2.11") =>
          Some(o % (n + "_" + sbv) % bas)
        case _ =>
          None
      }
    }
  )

  lazy val scalaz = Project(
    id = "scalaz",
    base = file("."),
    settings = standardSettings ++ unidocSettings ++ Seq[Sett](
      previousArtifact := None,
      // <https://github.com/scalaz/scalaz/issues/261>
      excludedProjects in unidoc in ScalaUnidoc += "typelevel",
      publishArtifact := false
    ),
    aggregate = Seq(core, concurrent, effect, example, iterv, iteratee, scalacheckBinding, tests, typelevel, xml)
  )

  // http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.scala-lang.modules%22%20
  val coreModuleDependencies211 = List[(String, String => String)] (
    "scala-parser-combinators" -> {
      case _ => "1.0.0"
    }
    ,
    "scala-xml"                -> {
      case _ => "1.0.0"
    }
  )

  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = standardSettings ++ buildInfoSettings ++ Seq[Sett](
      name := "scalaz-core",
      typeClasses := TypeClass.core,
      sourceGenerators in Compile <+= (sourceManaged in Compile) map {
        dir => Seq(generateTupleW(dir))
      },
      libraryDependencies ++= {
        if (scalaVersion.value.startsWith("2.11"))
          coreModuleDependencies211 map {
            case (a, v) => "org.scala-lang.modules" %% a % v(scalaVersion.value) intransitive()
          }
        else Nil
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

  lazy val iterv = Project(
    id = "iterv",
    base = file("iterv"),
    settings = standardSettings ++ Seq[Sett](
      name := "scalaz-iterv",
      OsgiKeys.fragmentHost := Some("org.scalaz.core"),
      OsgiKeys.exportPackage := Seq("scalaz;version=${Bundle-Version};-split-package:=first")
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
      previousArtifact := None,
      publishArtifact := false,
      sources in Compile := {
        val fs = (sources in Compile).value
        if (scalaVersion.value.contains("2.11.")) fs.filterNot(_.getName == "WordCount.scala") // See SI-8290
        else fs
      }
    )
  )

  lazy val scalacheckBinding = Project(
    id           = "scalacheck-binding",
    base         = file("scalacheck-binding"),
    dependencies = Seq(core, concurrent, typelevel, xml, iteratee),
    settings     = standardSettings ++ Seq[Sett](
      name := "scalaz-scalacheck-binding",
      libraryDependencies += "org.scalacheck" %% "scalacheck" % Dependencies.scalacheck(scalaVersion.value),
      conflictWarning in Global ~= { _.copy(failOnConflict = false) }, // workaround for 2.11
      osgiExport("scalaz.scalacheck")
    )
  )

  lazy val tests = Project(
    id = "tests",
    base = file("tests"),
    dependencies = Seq(core, iteratee, concurrent, effect, typelevel, xml, scalacheckBinding % "test"),
    settings = standardSettings ++ Seq[Sett](
      name := "scalaz-tests",
      publishArtifact := false,
      previousArtifact := None,
      sources in Test := {
        val fs = (sources in Test).value
        if (scalaVersion.value.contains("2.11.")) fs.filterNot(_.getName == "FuncTest.scala") // See SI-8290
        else fs
      }
    )
  )

  object Dependencies {
    def scalacheck(sv: String) =
      if (sv startsWith "2.11")
        "1.11.3"
      else
        "1.10.1"
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

  lazy val scalazMimaBasis =
    SettingKey[String]("scalaz-mima-basis", "Version of scalaz against which to run MIMA.")

  lazy val genTypeClasses = TaskKey[Seq[File]]("gen-type-classes")

  lazy val typeClasses = TaskKey[Seq[TypeClass]]("type-classes")

  lazy val genToSyntax = TaskKey[String]("gen-to-syntax")

  lazy val showDoc = TaskKey[Unit]("show-doc")

  lazy val typeClassTree = TaskKey[String]("type-class-tree", "Generates scaladoc formatted tree of type classes.")

  def generateTupleW(outputDir: File) = {
    val arities = 2 to 12

    def writeFileScalazPackage(fileName: String, source: String): File = {
      val file = (outputDir / "scalaz" / "syntax" / "std" / fileName).asFile
      IO.write(file, source)
      file
    }

    def double(s: String) = s + s

    val tuples: IndexedSeq[(String, String)] = for (arity: Int <- arities) yield {
      case class N(n: Int) {
        val alpha: String = ('A' + (n - 1)).toChar.toString
        val alpha2: String = alpha + alpha
        val element: String = "_" + n
      }
      val ns = (1 to arity) map N.apply
      def mapMkString(f: N => String): String = ns.map(f).mkString(", ")

      val tparams = mapMkString {
        n => n.alpha
      }
      val params = mapMkString {
        n => n.element
      }

      val ztparams = mapMkString {
        _ => "Z"
      }

      val mapallTParams = mapMkString {
        n => n.alpha2
      }
      val mapallParams = mapMkString {
        n => "%s: (%s => %s) = identity[%s] _".format(n.element, n.alpha, n.alpha2, n.alpha)
      }
      val mapallApply = mapMkString {
        n => "%s(value.%s)".format(n.element, n.element)
      }

      val pimp = """|
          |trait Tuple%dOps[%s] extends Ops[Tuple%d[%s]] {
          |  val value = self
          |  def fold[Z](f: => (%s) => Z): Z = {import value._; f(%s)}
          |  def toIndexedSeq[Z](implicit ev: value.type <:< Tuple%d[%s]): IndexedSeq[Z] = {val zs = ev(value); import zs._; IndexedSeq(%s)}
          |  def mapElements[%s](%s): (%s) = (%s)
          |}""".stripMargin.format(arity, tparams, arity, tparams, tparams, params, arity,
        ztparams, params,
        mapallTParams, mapallParams, mapallTParams, mapallApply
      )

      val conv = """implicit def ToTuple%dOps[%s](t: (%s)): Tuple%dOps[%s] = new { val self = t } with Tuple%dOps[%s]
          |""".stripMargin.format(arity, tparams, tparams, arity, tparams, arity, tparams)
      (pimp, conv)
    }

    val source = "package scalaz\npackage syntax\npackage std\n\n" +
      tuples.map(_._1).mkString("\n") +
      "\n\ntrait ToTupleOps {\n" +
         tuples.map("  " + _._2).mkString("\n") +
      "}"
    writeFileScalazPackage("TupleOps.scala", source)
  }

  def osgiExport(packs: String*) = OsgiKeys.exportPackage := packs.map(_ + ".*;version=${Bundle-Version}")
}

// vim: expandtab:ts=2:sw=2
