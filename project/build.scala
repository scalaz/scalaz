import sbt._
import Keys._

import GenTypeClass._

import java.awt.Desktop

import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

import com.jsuereth.sbtpgp.SbtPgp.autoImport.PgpKeys.{publishSigned, publishLocalSigned}

import com.typesafe.tools.mima.plugin.MimaKeys.{mimaPreviousArtifacts, mimaReportSignatureProblems}

object build {
  type Sett = Def.Setting[?]

  val rootNativeId = "rootNative"

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
    enableCrossBuild = true
  )

  lazy val setMimaVersion: ReleaseStep = { (st: State) =>
    val extracted = Project.extract(st)
    val (releaseV, _) = st.get(ReleaseKeys.versions).getOrElse(sys.error("impossible"))
    IO.write(extracted.get(releaseVersionFile), s"""\nThisBuild / build.scalazMimaBasis := "${releaseV}"\n""", append = true)
    reapply(Seq(ThisBuild / scalazMimaBasis := releaseV), st)
  }

  val kindProjectorVersion = SettingKey[String]("kindProjectorVersion")

  private def gitHash(): String = sys.process.Process("git rev-parse HEAD").lazyLines_!.head

  private val tagName = Def.setting{
    s"v${if (releaseUseGlobalVersion.value) (ThisBuild / version).value else version.value}"
  }
  private val tagOrHash = Def.setting{
    if(isSnapshot.value) gitHash() else tagName.value
  }

  val jsSettings = Def.settings(
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
    mimaPreviousArtifacts := Set.empty,
  )

  private val stdOptions = Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps",
    "-unchecked"
  )

  val unusedWarnOptions = Def.setting {
    scalaBinaryVersion.value match {
      case "3" =>
        Seq("-Wunused:imports")
      case _ =>
        Seq("-Ywarn-unused:imports")
    }
  }

  val oldLintOptions = Seq(
    "-Xlint:adapted-args",
    "-Xlint:_,-type-parameter-shadow,-missing-interpolator",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    // "-Yrangepos" https://github.com/scala/bug/issues/10706
  )

  def Scala212 = "2.12.21"
  def Scala213 = "2.13.18"
  def Scala3 = "3.8.4"

  val buildInfoPackageName = "scalaz"

  lazy val standardSettings: Seq[Sett] = Def.settings(
    organization := "org.scalaz",
    Seq(Compile, Test).map { scope =>
      (scope / unmanagedSourceDirectories) ++= {
        projectMatrixBaseDirectory.?.value.map { p =>
          val base = p / "src" / Defaults.nameForSrc(scope.name)
          val dir = CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((2, v)) if v <= 12 =>
              "scala-2.13-"
            case _ =>
              "scala-2.13+"
          }
          (base / dir).getAbsoluteFile
        }
      }
    },
    Compile / doc / sources := {
      scalaBinaryVersion.value match {
        case "3" =>
          // TODO OutOfMemoryError
          Nil
        case _ =>
          (Compile / doc / sources).value
      }
    },
    scalacOptions ++= stdOptions,
    scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((2, v)) if v <= 12 =>
        Seq(
          "-Xfuture",
          "-Ypartial-unification",
        )
    }.toList.flatten,
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) if v >= 12 =>
          Seq(
            "-opt:l:method,inline",
            "-opt-inline-from:scalaz.**"
          )
        case _ =>
          Nil
      }
    },
    scalacOptions ++= {
      scalaBinaryVersion.value match {
        case "3" =>
          Nil
        case _ =>
          oldLintOptions
      }
    },
    scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((0 | 3, _)) =>
        Seq(
          "-source", "3.0-migration",
          "-Ykind-projector",
        )
    }.toList.flatten,
    scalacOptions ++= unusedWarnOptions.value,
    Seq(Compile, Test).flatMap(c =>
      (c / console / scalacOptions) --= unusedWarnOptions.value
    ),

    (Compile / doc / scalacOptions) ++= {
      val base = (LocalRootProject / baseDirectory).value.getAbsolutePath
      Seq("-sourcepath", base, "-doc-source-url", "https://github.com/scalaz/scalaz/tree/" + tagOrHash.value + "€{FILE_PATH}.scala")
    },

    // retronym: I was seeing intermittent heap exhaustion in scalacheck based tests, so opting for determinism.
    Test / parallelExecution := false,
    genTypeClasses := {
      val s = streams.value
      typeClasses.value.flatMap { tc =>
        projectMatrixBaseDirectory.?.value.map { p =>
          val dir = p / "src" / "main" / "scala"
          typeclassSource(tc).sources.map(_.createOrUpdate(dir, s.log))
        }
      }.flatten
    },
    checkGenTypeClasses := {
      val classes = genTypeClasses.value
      if(classes.exists(_._1 != FileStatus.NoChange))
        sys.error(classes.groupBy(_._1).view.filterKeys(_ != FileStatus.NoChange).mapValues(_.map(_._2)).toMap.toString)
    },
    typeClasses := Seq(),
    genToSyntax := {
      val tcs = typeClasses.value
      val objects = tcs.map(tc => s"object ${ScalazUtil.initLower(tc.name)} extends To${tc.name}Syntax").mkString("\n")
      val all = "object all extends " + tcs.map(tc => s"To${tc.name}Syntax").mkString(" with ")
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
    scmInfo := Some(ScmInfo(
      browseUrl = uri("https://github.com/scalaz/scalaz"),
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
      val LICENSE_txt = (ThisBuild / baseDirectory).value / "LICENSE.txt"
      if (!LICENSE_txt.exists()) sys.error(s"cannot find license file at $LICENSE_txt")
      fileConverter.value.toVirtualFile(LICENSE_txt.toPath)
    },
    // kind-projector plugin
    kindProjectorVersion := "0.13.4",
    libraryDependencies ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((2, _)) =>
        Seq(
          compilerPlugin(("org.typelevel" % "kind-projector" % kindProjectorVersion.value).cross(CrossVersion.full))
        )
    }.toList.flatten
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

  lazy val jvm_js_settings = Seq(
    scalacOptions ++= {
      if (scalaVersion.value.startsWith("3.3.")) {
        Seq(
          "-Yfuture-lazy-vals",
          "-release:11",
        )
      } else if (scalaBinaryVersion.value == "3") {
        Nil
      } else {
        Seq(
          "-release:8",
        )
      }
    },
    (Compile / unmanagedSourceDirectories) += {
      (projectMatrixBaseDirectory.value / "jvm_js/src/main/scala/").getAbsoluteFile
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
        organization.value % s"${name.value}_native0.5_${scalaBinaryVersion.value}" % _
      }.toSet
    },
  )

  lazy val licenseFile = settingKey[HashedVirtualFileRef]("The license file to include in packaged artifacts")

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
