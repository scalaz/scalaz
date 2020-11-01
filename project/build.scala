import sbt._
import Keys._

import GenTypeClass._

import java.awt.Desktop

import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

import com.jsuereth.sbtpgp.SbtPgp.autoImport.PgpKeys.{publishSigned, publishLocalSigned}

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi

import sbtbuildinfo.BuildInfoPlugin.autoImport._

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._

import sbtdynver.DynVerPlugin.autoImport._

import xerial.sbt.Sonatype.autoImport._

import dotty.tools.sbtplugin.DottyPlugin.autoImport.{isDotty, isDottyJS, dottyLatestNightlyBuild}

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
      val (newState, value) = ex.runTask(publishTo in Global in ref, st)
      Classpaths.getPublishTo(value)
      newState
    },
    enableCrossBuild = true
  )

  val scalaCheckVersion = SettingKey[String]("scalaCheckVersion")
  val kindProjectorVersion = SettingKey[String]("kindProjectorVersion")

  private[this] def gitHash(): String = sys.process.Process("git rev-parse HEAD").lineStream_!.head

  private[this] val tagName = Def.setting{
    s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
  }
  private[this] val tagOrHash = Def.setting{
    if(isSnapshot.value) gitHash() else tagName.value
  }

  val scalajsProjectSettings = Def.settings(
    scalacOptions ++= {
      val a = (baseDirectory in LocalRootProject).value.toURI.toString
      val g = "https://raw.githubusercontent.com/scalaz/scalaz/" + tagOrHash.value
      if (isDottyJS.value) {
        // TODO
        // https://github.com/lampepfl/dotty/blob/4c99388e77be12ee6cc/compiler/src/dotty/tools/backend/sjs/JSPositions.scala#L64-L69
        Nil
      } else {
        Seq(s"-P:scalajs:mapSourceURI:$a->$g/")
      }
    }
  )

  lazy val notPublish = Seq(
    publishArtifact := false,
    publish := {},
    publishLocal := {},
    publishSigned := {},
    publishLocalSigned := {}
  )

  // avoid move files
  object ScalazCrossType extends sbtcrossproject.CrossType {
    override def projectDir(crossBase: File, projectType: String) =
      crossBase / projectType

    override def projectDir(crossBase: File, projectType: sbtcrossproject.Platform) = {
      val dir = projectType match {
        case JVMPlatform => "jvm"
        case JSPlatform => "js"
      }
      crossBase / dir
    }

    def shared(projectBase: File, conf: String) =
      projectBase.getParentFile / "src" / conf / "scala"

    def scala2(projectBase: File, conf: String) =
      projectBase.getParentFile / "src" / conf / "scala-2"

    def scala3(projectBase: File, conf: String) =
      projectBase.getParentFile / "src" / conf / "scala-3"

    override def sharedSrcDir(projectBase: File, conf: String) =
      Some(shared(projectBase, conf))
  }

  val unusedWarnOptions = Def.setting {
    Seq("-Ywarn-unused:imports")
  }

  private def Scala213 = "2.13.3"

  private[this] val buildInfoPackageName = "scalaz"

  lazy val standardSettings: Seq[Sett] = Def.settings(
    organization := "org.scalaz",
    mappings in (Compile, packageSrc) ++= (managedSources in Compile).value.map{ f =>
      // https://github.com/sbt/sbt-buildinfo/blob/v0.7.0/src/main/scala/sbtbuildinfo/BuildInfoPlugin.scala#L58
      val buildInfoDir = "sbt-buildinfo"
      val path = if(f.getAbsolutePath.contains(buildInfoDir)) {
        (file(buildInfoPackageName) / f.relativeTo((sourceManaged in Compile).value / buildInfoDir).get.getPath).getPath
      } else {
        f.relativeTo((sourceManaged in Compile).value).get.getPath
      }
      (f, path)
    },
    commands += Command.command("SetDottyNightlyVersion") {
      s"""++ ${dottyLatestNightlyBuild.get}!""" :: _
    },
    scalaVersion := Scala213,
    crossScalaVersions := Seq(Scala213),
    commands += Command.command("setVersionUseDynver") { state =>
      val extracted = Project extract state
      val out = extracted get dynverGitDescribeOutput
      val date = extracted get dynverCurrentDate
      s"""set version in ThisBuild := "${out.sonatypeVersion(date)}" """ :: state
    },
    resolvers ++= (if (scalaVersion.value.endsWith("-SNAPSHOT")) List(Opts.resolver.sonatypeSnapshots) else Nil),
    fullResolvers ~= {_.filterNot(_.name == "jcenter")}, // https://github.com/sbt/sbt/issues/2217
    scalaCheckVersion := "1.15.0",
    Seq(Compile, Test).map { scope =>
      unmanagedSourceDirectories in scope ++= {
        val dir = Defaults.nameForSrc(scope.name)
        val base = ScalazCrossType.shared(baseDirectory.value, dir).getParentFile
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, _)) =>
            Seq(ScalazCrossType.scala2(baseDirectory.value, dir))
          case Some((0 | 3, _)) =>
            Seq(ScalazCrossType.scala3(baseDirectory.value, dir))
          case _ =>
            Nil
        }
      }
    },
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked"
    ),
    scalacOptions ++= {
      val common = "implicitConversions,higherKinds,existentials"
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((0 | 3, _)) =>
          Seq(
            "-Ykind-projector",
            s"-language:Scala2Compat,$common",
            "-rewrite",
          )
        case _ =>
          Seq(
            "-Xsource:3",
            "-Xlint:_,-type-parameter-shadow,-missing-interpolator",
            "-Ywarn-dead-code",
            "-Ywarn-numeric-widen",
            "-Ywarn-value-discard",
            "-Xlint:adapted-args",
            "-opt:l:method,inline",
            "-opt-inline-from:scalaz.**",
            s"-language:$common",
          ) ++ unusedWarnOptions.value
      }
    },
    Seq(Compile, Test).flatMap(c =>
      scalacOptions in (c, console) --= unusedWarnOptions.value
    ),

    scala213_pre_cross_setting,

    scalacOptions in (Compile, doc) := {
      val tag = tagOrHash.value
      val base = (baseDirectory in LocalRootProject).value.getAbsolutePath
      val options = (scalacOptions in (Compile, doc)).value
      if (isDotty.value) {
        Nil
      } else {
        options ++ Seq("-sourcepath", base, "-doc-source-url", "https://github.com/scalaz/scalaz/tree/" + tag + "€{FILE_PATH}.scala")
      }
    },
    sources in (Compile, doc) := {
      val src = (sources in (Compile, doc)).value
      if (isDotty.value) {
        Nil
      } else {
        src
      }
    },

    // retronym: I was seeing intermittent heap exhaustion in scalacheck based tests, so opting for determinism.
    parallelExecution in Test := false,
    genTypeClasses := {
      val s = streams.value
      typeClasses.value.flatMap { tc =>
        val dir = ScalazCrossType.shared(baseDirectory.value, "main")
        val scala2dir = ScalazCrossType.scala2(baseDirectory.value, "main")
        val scala3dir = ScalazCrossType.scala3(baseDirectory.value, "main")
        val t = typeclassSource(tc)

        List(
          t.sources.map(_.createOrUpdate(dir, s.log)),
          t.scala2sources.map(_.createOrUpdate(scala2dir, s.log)),
          t.scala3sources.map(_.createOrUpdate(scala3dir, s.log)),
        ).flatten
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
      val out = (target in doc in Compile).value
      val index = out / "index.html"
      if (index.exists()) Desktop.getDesktop.open(out / "index.html")
    },

    credentialsSetting,
    publishTo := sonatypePublishToBundle.value,
    sonatypeBundleDirectory := {
      (LocalRootProject / target).value / "sonatype-staging" / (ThisBuild / version).value
    },
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
      releaseStepCommandAndRemaining("set ThisBuild / useSuperShell := false"),
      publishSignedArtifacts,
      releaseStepCommandAndRemaining("set ThisBuild / useSuperShell := true"),
      releaseStepCommandAndRemaining("sonatypeBundleRelease"),
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),
    releaseTagName := tagName.value,
    pomIncludeRepository := {
      x => false
    },
    scmInfo := Some(ScmInfo(
      browseUrl = url("https://github.com/scalaz/scalaz"),
      connection = "scm:git:git@github.com:scalaz/scalaz.git"
    )),
    pomExtra := (
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
      val LICENSE_txt = (baseDirectory in ThisBuild).value / "LICENSE.txt"
      if (!LICENSE_txt.exists()) sys.error(s"cannot find license file at $LICENSE_txt")
      LICENSE_txt
    },
    // kind-projector plugin
    kindProjectorVersion := "0.11.0",
    libraryDependencies ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((2, _)) =>
        Seq(
          compilerPlugin("org.typelevel" % "kind-projector" % kindProjectorVersion.value cross CrossVersion.full)
        )
    }.toList.flatten
  ) ++ Seq(packageBin, packageDoc, packageSrc).flatMap {
    // include LICENSE.txt in all packaged artifacts
    inTask(_)(Seq(mappings in Compile += licenseFile.value -> "LICENSE"))
  } ++ SbtOsgi.projectSettings ++ Seq[Sett](
    OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
  )

  private[this] val jvm_js_settings = Seq(
    unmanagedSourceDirectories in Compile += {
      baseDirectory.value.getParentFile / "jvm_js/src/main/scala/"
    }
  )

  private[this] val scala213_pre_cross_setting = {
    // sbt wants `scala-2.13.0-M1`, `scala-2.13.0-M2`, ... (sbt/sbt#2819)
    // @fommil tells me we could use sbt-sensible for this
    unmanagedSourceDirectories in Compile ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2L, minor)) =>
          Some((baseDirectory in Compile).value.getParentFile / s"src/main/scala-2.$minor")
        case _               =>
          None
      }
    }
  }

  lazy val core = crossProject(JSPlatform, JVMPlatform).crossType(ScalazCrossType)
    .settings(standardSettings: _*)
    .settings(
      name := "scalaz-core",
      sourceGenerators in Compile += (sourceManaged in Compile).map{
        dir => Seq(GenerateTupleW(dir), TupleNInstances(dir))
      }.taskValue,
      buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
      buildInfoPackage := buildInfoPackageName,
      buildInfoObject := "ScalazBuildInfo",
      osgiExport("scalaz"),
      OsgiKeys.importPackage := Seq("javax.swing;resolution:=optional", "*"))
    .enablePlugins(sbtbuildinfo.BuildInfoPlugin)
    .jsSettings(
      jvm_js_settings,
      scalajsProjectSettings,
    )
    .jvmSettings(
      jvm_js_settings,
      typeClasses := TypeClass.core
    )

  lazy val effect = crossProject(JSPlatform, JVMPlatform).crossType(ScalazCrossType)
    .settings(standardSettings: _*)
    .settings(
      name := "scalaz-effect",
      osgiExport("scalaz.effect", "scalaz.std.effect", "scalaz.syntax.effect"))
    .dependsOn(core)
    .jsSettings(scalajsProjectSettings : _*)
    .jvmSettings(
      typeClasses := TypeClass.effect
    )

  lazy val iteratee = crossProject(JSPlatform, JVMPlatform).crossType(ScalazCrossType)
    .settings(standardSettings: _*)
    .settings(
      name := "scalaz-iteratee",
      osgiExport("scalaz.iteratee"))
    .dependsOn(core, effect)
    .jsSettings(scalajsProjectSettings : _*)

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

  lazy val genTypeClasses = taskKey[Seq[(FileStatus, File)]]("")

  lazy val typeClasses = taskKey[Seq[TypeClass]]("")

  lazy val genToSyntax = taskKey[String]("")

  lazy val showDoc = taskKey[Unit]("")

  lazy val typeClassTree = taskKey[String]("Generates scaladoc formatted tree of type classes.")

  lazy val checkGenTypeClasses = taskKey[Unit]("")

  def osgiExport(packs: String*) = OsgiKeys.exportPackage := packs.map(_ + ".*;version=${Bundle-Version}")
}

// vim: expandtab:ts=2:sw=2
