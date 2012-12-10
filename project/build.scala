import collection.immutable.IndexedSeq
import java.awt.Desktop
import sbt._
import Keys._
import GenTypeClass._
import Project.Setting
import com.typesafe.sbtosgi.OsgiPlugin._

object build extends Build {
  type Sett = Project.Setting[_]

  lazy val standardSettings: Seq[Sett] = Defaults.defaultSettings ++ sbtrelease.ReleasePlugin.releaseSettings ++ Seq[Sett](
    organization := "org.scalaz",
    scalaVersion := "2.9.2",
    crossScalaVersions := Seq("2.9.2", "2.10.0-RC5"),
    crossVersion := CrossVersion.full,
    resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases",

    scalacOptions <++= (scalaVersion) map { sv =>
      val versionDepOpts =
        if (sv.contains("2.10"))
          // does not contain -deprecation (because of ClassManifest)
          // contains -language:postfixOps (because 1+ as a parameter to a higher-order function is treated as a postfix op)
          Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps")
        else
          Seq("-Ydependent-method-types", "-deprecation")

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
    // useGpg := false,
    // useGpgAgent := false,
    publishSetting,
    publishArtifact in Test := false,
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
    settings = standardSettings ++ Unidoc.settings,
    aggregate = Seq(core, concurrent, effect, example, iterv, iteratee, testlib, typelevel, xml, tests)
  )

  lazy val core = Project(
    id = "core",
    base = file("core"),
    settings = standardSettings ++ Seq[Sett](
      name := "scalaz-core",
      typeClasses := TypeClass.core,
      (sourceGenerators in Compile) <+= (sourceManaged in Compile) map {
        dir => Seq(generateTupleW(dir))
      },
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
      OsgiKeys.fragmentHost := Some("org.scalaz.core")
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
      osgiExport("scalaz.example")
    )
  )

  lazy val testlib = Project(
    id           = "testlib",
    base         = file("testlib"),
    dependencies = Seq(core, concurrent, typelevel),
    settings     = standardSettings ++ Seq[Sett](
      name := "scalaz-testlib",
      libraryDependencies ++= Seq(
        "org.specs2" %% "specs2" % "1.12.3" cross CrossVersion.full,
        "org.scalacheck" %% "scalacheck" % "1.10.0" cross CrossVersion.full
      ),
      osgiExport("scalaz.testlib")
    )
  )

  lazy val tests = Project(
    id = "tests",
    base = file("tests"),
    dependencies = Seq(core, iteratee, concurrent, effect, typelevel, testlib % "test"),
    settings = standardSettings ++ Seq[Sett](
      name := "scalaz-tests",
      publishTo := None,
      libraryDependencies ++= Seq(
        "org.specs2" %% "specs2" % "1.12.3" % "test" cross CrossVersion.full,
        "org.scalacheck" %% "scalacheck" % "1.10.0" % "test" cross CrossVersion.full
      )
    )
  )

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
