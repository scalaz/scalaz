import sbt._
import Keys._

import GenTypeClass._

import java.awt.Desktop

import sbtprojectmatrix.ProjectMatrixPlugin.autoImport.*

import sbtrelease._
import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

import com.jsuereth.sbtpgp.SbtPgp.autoImport.PgpKeys.{publishSigned, publishLocalSigned}

import sbtbuildinfo.BuildInfoPlugin.autoImport._

import com.typesafe.tools.mima.core.ProblemFilters
import com.typesafe.tools.mima.core.IncompatibleSignatureProblem
import com.typesafe.tools.mima.core.InheritedNewAbstractMethodProblem
import com.typesafe.tools.mima.plugin.MimaPlugin
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaPreviousArtifacts, mimaReportSignatureProblems, mimaBinaryIssueFilters}

import scalanative.sbtplugin.ScalaNativePlugin.autoImport._

import sbtdynver.DynVerPlugin.autoImport._

object build {
  type Sett = Def.Setting[?]

  lazy val publishSignedArtifacts = ReleaseStep(
    action = st => {
      val extracted = st.extract
      val ref = extracted.get(thisProjectRef)
      extracted.runAggregated(ref / (Global / publishSigned), st)
    },
    check = st => {
      // getPublishTo fails if no publish repository is set up.
      val ex = st.extract
      val ref = ex.get(thisProjectRef)
      val (newState, value) = ex.runTask(ref / (Global / publishTo), st)
      Classpaths.getPublishTo(value)
      newState
    },
    enableCrossBuild = false
  )

  lazy val setMimaVersion: ReleaseStep = { (st: State) =>
    val extracted = Project.extract(st)
    val (releaseV, _) = st.get(ReleaseKeys.versions).getOrElse(sys.error("impossible"))
    IO.write(extracted.get(releaseVersionFile), s"""\nThisBuild / build.scalazMimaBasis := "${releaseV}"\n""", append = true)
    reapply(Seq(ThisBuild / scalazMimaBasis := releaseV), st)
  }

  val kindProjectorVersion = SettingKey[String]("kindProjectorVersion")

  private def gitHash(): String = sys.process.Process("git rev-parse HEAD").lineStream_!.head

  private val tagName = Def.setting{
    s"v${if (releaseUseGlobalVersion.value) (ThisBuild / version).value else version.value}"
  }
  private val tagOrHash = Def.setting{
    if(isSnapshot.value) gitHash() else tagName.value
  }

  val scalajsProjectSettings = Def.settings(
    platformSrcDirSetting("js"),
    scalacOptions += {
      val a = (LocalRootProject / baseDirectory).value.toURI.toString
      val g = "https://raw.githubusercontent.com/scalaz/scalaz/" + tagOrHash.value

      val key = CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          "-scalajs-mapSourceURI"
        case _ =>
          "-P:scalajs:mapSourceURI"
      }
      s"${key}:$a->$g/"
    },
    mimaPreviousArtifacts := {
      scalazMimaBasis.?.value.map {
        organization.value % s"${name.value}_sjs1_${scalaBinaryVersion.value}" % _
      }.toSet
    }
  )

  lazy val notPublish = Seq(
    publishArtifact := false,
    publish := {},
    publishLocal := {},
    publishSigned := {},
    publishLocalSigned := {},
    mimaPreviousArtifacts := Set.empty
  )

  def Scala212 = "2.12.21"
  def Scala213 = "2.13.18"
  def Scala3 = "3.3.8"

  val buildInfoPackageName = "scalaz"

  lazy val standardSettings: Seq[Sett] = Def.settings(
    Seq(12, 13).flatMap { scalaV =>
      Seq(Compile, Test).map { scope =>
        (scope / unmanagedSourceDirectories) ++= {
          projectMatrixBaseDirectory.?.value.map { p =>
            val base = p / "src" / Defaults.nameForSrc(scope.name)
            val dir = CrossVersion.partialVersion(scalaVersion.value) match {
              case Some((x, y)) if ((x == 2) && (y >= scalaV)) || (x >= 3) =>
                s"scala-2.${scalaV}+"
              case _ =>
                s"scala-2.${scalaV}-"
            }
            (base / dir).getAbsoluteFile
          }
        }
      }
    },
    organization := "org.scalaz",
    Compile / doc / sources := {
      scalaBinaryVersion.value match {
        case "3" =>
          // TODO OutOfMemoryError
          Nil
        case _ =>
          (Compile / doc / sources).value
      }
    },
    (Compile / packageSrc / mappings) ++= (Compile / managedSources).value.map{ f =>
      // https://github.com/sbt/sbt-buildinfo/blob/v0.7.0/src/main/scala/sbtbuildinfo/BuildInfoPlugin.scala#L58
      val buildInfoDir = "sbt-buildinfo"
      val path = if(f.getAbsolutePath.contains(buildInfoDir)) {
        (file(buildInfoPackageName) / f.relativeTo((Compile / sourceManaged).value / buildInfoDir).get.getPath).getPath
      } else {
        f.relativeTo((Compile / sourceManaged).value).get.getPath
      }
      (f, path)
    },
    commands += Command.command("setVersionUseDynver") { state =>
      val extracted = Project.extract(state)
      val out = extracted.get(dynverGitDescribeOutput)
      val date = extracted.get(dynverCurrentDate)
      s"""set ThisBuild / version := "${out.sonatypeVersion(date)}" """ :: state
    },
    scalacOptions ++= Seq(
      // contains -language:postfixOps (because 1+ as a parameter to a higher-order function is treated as a postfix op)
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps",
      "-unchecked"
    ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2,v)) if v >= 12 => Seq("-opt:l:method")
      case _ => Nil
    }),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v <= 12 =>
          Seq("-Xfuture")
        case _ =>
          Nil
      }
    },
    scalacOptions ++= {
      scalaBinaryVersion.value match {
        case "3" =>
          Seq(
            "-source", "3.0-migration",
            "-Ykind-projector",
          )
        case _ =>
          Nil
      }
    },
    (Compile / doc / scalacOptions) ++= {
      val base = (LocalRootProject / baseDirectory).value.getAbsolutePath
      Seq("-sourcepath", base, "-doc-source-url", "https://github.com/scalaz/scalaz/tree/" + tagOrHash.value + "€{FILE_PATH}.scala")
    },

    // retronym: I was seeing intermittent heap exhaustion in scalacheck based tests, so opting for determinism.
    Test / parallelExecution := false,
    genTypeClasses := {
      val s = streams.value
      typeClasses.value.flatMap { tc =>
        name.value match {
          case ConcurrentName =>
            val dir = (Compile / scalaSource).value
            typeclassSource(tc).sources.map(_.createOrUpdate(dir, s.log))
          case _ =>
            projectMatrixBaseDirectory.?.value.map { p =>
              val dir = p / "src" / "main" / "scala"
              typeclassSource(tc).sources.map(_.createOrUpdate(dir, s.log))
            }.toSeq.flatten
        }
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
      val objects = tcs.map(tc => "object %s extends To%sSyntax".format(ScalazUtil.initLower(tc.name), tc.name)).mkString("\n")
      val all = "object all extends " + tcs.map(tc => "To%sSyntax".format(tc.name)).mkString(" with ")
      objects + "\n\n" + all
    },
    typeClassTree := {
      typeClasses.value.map(_.doc).mkString("\n")
    },

    (Compile / showDoc) := {
      val _ = (Compile / doc).value
      val out = (Compile / doc / target).value
      val index = out / "index.html"
      if (index.exists()) Desktop.getDesktop.open(out / "index.html")
    },

    credentialsSetting,
    publishTo := localStaging.value,
    Test / publishArtifact := false,

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
      releaseStepCommandAndRemaining("sonaRelease"),
      setNextVersion,
      setMimaVersion,
      commitNextVersion,
      pushChanges
    ),
    releaseTagName := tagName.value,
    pomIncludeRepository := {
      x => false
    },
    pomExtra := (
        <scm>
          <url>git@github.com:scalaz/scalaz.git</url>
          <connection>scm:git:git@github.com:scalaz/scalaz.git</connection>
          <tag>{tagOrHash.value}</tag>
        </scm>
        <url>http://scalaz.org</url>
        <licenses>
          <license>
            <name>BSD-style</name>
            <url>https://opensource.org/licenses/BSD-3-Clause</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
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
                <url>https://github.com/{id}</url>
              </developer>
          }
        }
        </developers>
      ),

    licenseFile := {
      val LICENSE_txt = (ThisBuild / baseDirectory).value / "LICENSE.txt"
      if (!LICENSE_txt.exists()) sys.error(s"cannot find license file at $LICENSE_txt")
      LICENSE_txt
    },
    // kind-projector plugin
    kindProjectorVersion := "0.13.4",
    libraryDependencies ++= {
      if (scalaBinaryVersion.value == "3") {
        Nil
      } else {
        Seq(compilerPlugin(("org.typelevel" % "kind-projector" % kindProjectorVersion.value).cross(CrossVersion.full)))
      }
    }
  ) ++ Seq(packageBin, packageDoc, packageSrc).flatMap {
    // include LICENSE.txt in all packaged artifacts
    Project.inTask(_)(Seq((Compile / mappings) += licenseFile.value -> "LICENSE"))
  } ++ Def.settings(
    ThisBuild / mimaReportSignatureProblems := (scalaBinaryVersion.value != "3"),
    mimaPreviousArtifacts := {
      scalazMimaBasis.?.value.map {
        organization.value % s"${name.value}_${scalaBinaryVersion.value}" % _
      }.toSet
    }
  )

  private def platformSrcDirSetting(d: String) =
    Seq(Compile, Test).map { x =>
      x / unmanagedSourceDirectories += {
        (projectMatrixBaseDirectory.value / d / "src" / Defaults.nameForSrc(x.name) / "scala").getAbsoluteFile
      }
    }

  val jvmSettings = Def.settings(
    platformSrcDirSetting("jvm"),
  )

  val nativeSettings = Def.settings(
    platformSrcDirSetting("native"),
    Compile / doc / scalacOptions --= {
      // TODO remove this workaround
      // https://github.com/scala-native/scala-native/issues/2503
      if (scalaBinaryVersion.value == "3") {
        (Compile / doc / scalacOptions).value.filter(_.contains("-Xplugin"))
      } else {
        Nil
      }
    },
    mimaPreviousArtifacts := {
      scalazMimaBasis.?.value.map {
        organization.value % s"${name.value}_native0.4_${scalaBinaryVersion.value}" % _
      }.toSet
    }
  )

  final val ConcurrentName = "scalaz-concurrent"

  lazy val credentialsSetting = credentials ++= {
    val name = "Sonatype Nexus Repository Manager"
    val realm = "oss.sonatype.org"
    (
      sys.props.get("build.publish.user"),
      sys.props.get("build.publish.password"),
      sys.env.get("SONATYPE_USERNAME"),
      sys.env.get("SONATYPE_PASSWORD")
    ) match {
      case (Some(user), Some(pass), _, _)  => Seq(Credentials(name, realm, user, pass))
      case (_, _, Some(user), Some(pass))  => Seq(Credentials(name, realm, user, pass))
      case _                           =>
        val ivyFile = Path.userHome / ".ivy2" / ".credentials"
        val m2File = Path.userHome / ".m2" / "credentials"
        if (ivyFile.exists()) Seq(Credentials(ivyFile))
        else if (m2File.exists()) Seq(Credentials(m2File))
        else Nil
    }
  }

  lazy val licenseFile = settingKey[File]("The license file to include in packaged artifacts")

  lazy val scalazMimaBasis = settingKey[String]("Version of scalaz against which to run MIMA.")

  @transient
  lazy val genTypeClasses = taskKey[Seq[(FileStatus, File)]]("")

  @transient
  lazy val typeClasses = taskKey[Seq[TypeClass]]("")

  @transient
  lazy val genToSyntax = taskKey[String]("")

  @transient
  lazy val showDoc = taskKey[Unit]("")

  @transient
  lazy val typeClassTree = taskKey[String]("Generates scaladoc formatted tree of type classes.")

  @transient
  lazy val checkGenTypeClasses = taskKey[Unit]("")
}

// vim: expandtab:ts=2:sw=2
