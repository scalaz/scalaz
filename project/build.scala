import sbt._
import Keys._

import GenTypeClass._

import java.awt.Desktop

import sbtrelease._
import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

import com.typesafe.sbt.pgp.PgpKeys._

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi._

import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts

object build {
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

    val (releaseV, _) = st.get(ReleaseKeys.versions).getOrElse(sys.error("impossible"))
    IO.write(extracted get releaseVersionFile, s"""\nbuild.scalazMimaBasis in ThisBuild := "${releaseV}"\n""", append = true)
    reapply(Seq(scalazMimaBasis in ThisBuild := releaseV), st)
  }

  val scalacheckVersion = SettingKey[String]("scalacheckVersion")

  private def gitHash = sys.process.Process("git rev-parse HEAD").lines_!.head

  // no generic signatures for scala 2.10.x and 2.9.x, see SI-7932, #571 and #828
  def scalac210Options = Seq("-Yno-generic-signatures")

  lazy val standardSettings: Seq[Sett] = Seq[Sett](
    unmanagedSourceDirectories in Compile += {
      val base = (sourceDirectory in Compile).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 12 =>
          base / "scala-2.12+"
        case _ =>
          base / "scala-2.12-"
      }
    },
    organization := "org.scalaz",
    mappings in (Compile, packageSrc) ++= (managedSources in Compile).value.map{ f =>
      (f, f.relativeTo((sourceManaged in Compile).value).get.getPath)
    },
    scalaVersion := "2.10.6",
    crossScalaVersions := Seq("2.9.3", "2.10.6", "2.11.11"),
    resolvers ++= (if (scalaVersion.value.endsWith("-SNAPSHOT")) List(Opts.resolver.sonatypeSnapshots) else Nil),
    fullResolvers ~= {_.filterNot(_.name == "jcenter")}, // https://github.com/sbt/sbt/issues/2217
    scalacOptions ++= {
      val sv = scalaVersion.value
      val versionDepOpts =
        if (sv startsWith "2.9")
          Seq("-Ydependent-method-types", "-deprecation")
        else
          // does not contain -deprecation (because of ClassManifest)
          // contains -language:postfixOps (because 1+ as a parameter to a higher-order function is treated as a postfix op)
          Seq("-feature", "-Xfuture", "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps")

      Seq("-unchecked") ++ versionDepOpts
    } ++ (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 10 => scalac210Options
      case _ => Nil
    }),

    scalacOptions in (Compile, doc) ++= {
      val bd = (baseDirectory in LocalRootProject).value
      val tagOrBranch = if(isSnapshot.value) gitHash else ("v" + version.value)
      Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", "https://github.com/scalaz/scalaz/tree/" + tagOrBranch + "€{FILE_PATH}.scala")
    },

    // retronym: I was seeing intermittent heap exhaustion in scalacheck based tests, so opting for determinism.
    parallelExecution in Test := false,
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1"),

    (unmanagedClasspath in Compile) += Attributed.blank(file("dummy")),

    genTypeClasses := {
      typeClasses.value.flatMap {
        tc =>
          val typeClassSource0 = typeclassSource(tc)
          typeClassSource0.sources.map(_.createOrUpdate((scalaSource in Compile).value, streams.value.log))
      }
    },
    checkGenTypeClasses := {
      val classes = genTypeClasses.value
      if(classes.exists(_._1 != FileStatus.NoChange))
        sys.error(classes.groupBy(_._1).filterKeys(_ != FileStatus.NoChange).mapValues(_.map(_._2)).toString)
    },
    typeClasses := Seq(),
    genToSyntax := {
      val tcs = typeClasses.value
      val objects = tcs.map(tc => "object %s extends To%sSyntax".format(Util.initLower(tc.name), tc.name)).mkString("\n")
      val all = "object all extends " + tcs.map(tc => "To%sSyntax".format(tc.name)).mkString(" with ")
      objects + "\n\n" + all
    },
    typeClassTree := {
      typeClasses.value.map(_.doc).mkString("\n")
    },

    showDoc in Compile := {
      val _ = (doc in Compile).value
      val index = (target in doc in Compile).value / "index.html"
      if (index.exists()) Desktop.getDesktop.open(index)
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

    scalacheckVersion := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 13 =>
          "1.12.6"
        case Some((2, 12)) =>
          "1.11.6"
        case _ =>
          "1.11.4"
      }
    },
    autoAPIMappings := PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){
      case Some((2, v)) if v < 10 => false
    }.getOrElse(true),
    apiMappings := Map.empty,
    pomIncludeRepository := {
      x => false
    },
    pomExtra := (
      <url>http://scalaz.org</url>
        <licenses>
          <license>
            <name>BSD-style</name>
            <url>http://opensource.org/licenses/BSD-3-Clause</url>
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
    mimaPreviousArtifacts := scalazMimaBasis.?.value.map { bas =>
      organization.value % (name.value + "_" + scalaBinaryVersion.value) % bas
    }.toSet
  )

  // http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.scala-lang.modules%22%20
  val scalaParserCombinatorsVersion = SettingKey[String]("scalaParserCombinatorsVersion")
  val scalaXmlVersion = SettingKey[String]("scalaXmlVersion")

  lazy val publishSetting = publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }

  lazy val credentialsSetting = credentials += {
    Seq("build.publish.user", "build.publish.password") map sys.props.get match {
      case Seq(Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
      case _                           =>
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }
  }

  lazy val scalazMimaBasis =
    SettingKey[String]("scalaz-mima-basis", "Version of scalaz against which to run MIMA.")

  lazy val genTypeClasses = TaskKey[Seq[(FileStatus, File)]]("gen-type-classes")

  lazy val typeClasses = TaskKey[Seq[TypeClass]]("type-classes")

  lazy val genToSyntax = TaskKey[String]("gen-to-syntax")

  lazy val showDoc = TaskKey[Unit]("show-doc")

  lazy val typeClassTree = TaskKey[String]("type-class-tree", "Generates scaladoc formatted tree of type classes.")

  lazy val checkGenTypeClasses = TaskKey[Unit]("check-gen-type-classes")

  def osgiExport(packs: String*) = OsgiKeys.exportPackage := packs.map(_ + ".*;version=${Bundle-Version}")
}

// vim: expandtab:ts=2:sw=2
