import sbt._
import Keys._

import GenTypeClass._

import java.awt.Desktop

import sbtrelease._
import sbtrelease.ReleasePlugin.autoImport._
import sbtrelease.ReleaseStateTransformations._
import sbtrelease.Utilities._

import com.jsuereth.sbtpgp.SbtPgp.autoImport.PgpKeys.{publishSigned, publishLocalSigned}

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi

import sbtbuildinfo.BuildInfoPlugin.autoImport._

import com.typesafe.tools.mima.core.ProblemFilters
import com.typesafe.tools.mima.core.IncompatibleSignatureProblem
import com.typesafe.tools.mima.core.InheritedNewAbstractMethodProblem
import com.typesafe.tools.mima.plugin.MimaPlugin
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaPreviousArtifacts, mimaReportSignatureProblems, mimaBinaryIssueFilters}

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import scalanativecrossproject.ScalaNativeCrossPlugin.autoImport._
import scalanative.sbtplugin.ScalaNativePlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._

import sbtdynver.DynVerPlugin.autoImport._

import xerial.sbt.Sonatype.autoImport._

object build {
  type Sett = Def.Setting[_]

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

  lazy val setMimaVersion: ReleaseStep = { st: State =>
    val extracted = Project.extract(st)
    val (releaseV, _) = st.get(ReleaseKeys.versions).getOrElse(sys.error("impossible"))
    IO.write(extracted get releaseVersionFile, s"""\nThisBuild / build.scalazMimaBasis := "${releaseV}"\n""", append = true)
    reapply(Seq(ThisBuild / scalazMimaBasis := releaseV), st)
  }

  val scalaCheckVersion_1_15 = SettingKey[String]("scalaCheckVersion_1_15")
  val scalaCheckGroupId = SettingKey[String]("scalaCheckGroupId")
  val kindProjectorVersion = SettingKey[String]("kindProjectorVersion")

  private[this] def gitHash(): String = sys.process.Process("git rev-parse HEAD").lineStream_!.head

  // no generic signatures for scala 2.10.x, see SI-7932, #571 and #828
  def scalac210Options = Seq("-Yno-generic-signatures")

  private[this] val tagName = Def.setting{
    s"v${if (releaseUseGlobalVersion.value) (ThisBuild / version).value else version.value}"
  }
  private[this] val tagOrHash = Def.setting{
    if(isSnapshot.value) gitHash() else tagName.value
  }

  val scalajsProjectSettings = Seq[Sett](
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
      if ((scalaBinaryVersion.value == "3") && (scalazMimaBasis.?.value == Some("7.2.33"))) {
        Set.empty
      } else {
        scalazMimaBasis.?.value.map {
          organization.value % s"${name.value}_sjs1_${scalaBinaryVersion.value}" % _
        }.toSet
      }
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

  // avoid move files
  object ScalazCrossType extends sbtcrossproject.CrossType {
    override def projectDir(crossBase: File, projectType: String) =
      crossBase / projectType

    override def projectDir(crossBase: File, projectType: sbtcrossproject.Platform) = {
      val dir = projectType match {
        case JVMPlatform => "jvm"
        case JSPlatform => "js"
        case NativePlatform => "native"
      }
      crossBase / dir
    }

    def shared(projectBase: File, conf: String) =
      projectBase.getParentFile / "src" / conf / "scala"

    override def sharedSrcDir(projectBase: File, conf: String) =
      Some(shared(projectBase, conf))
  }

  private def Scala211 = "2.11.12"
  private def Scala212 = "2.12.15"
  private def Scala213 = "2.13.8"
  private def Scala3 = "3.1.0"

  private[this] val buildInfoPackageName = "scalaz"

  lazy val standardSettings: Seq[Sett] = Def.settings(
    Seq(12, 13).flatMap { scalaV =>
      Seq((Compile, "main"), (Test, "test")).map { case (scope, dir) =>
        (scope / unmanagedSourceDirectories) += {
          val base = ScalazCrossType.shared(baseDirectory.value, dir).getParentFile
          CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((x, y)) if ((x == 2) && (y >= scalaV)) || (x >= 3) =>
              base / s"scala-2.${scalaV}+"
            case _ =>
              base / s"scala-2.${scalaV}-"
          }
        }
      }
    },
    organization := "org.scalaz",
    Seq(Compile, Test).map { scope =>
      (scope / unmanagedSourceDirectories) += {
        val dir = Defaults.nameForSrc(scope.name)
        val base = ScalazCrossType.shared(baseDirectory.value, dir).getParentFile
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, v)) if v <= 12 =>
            base / "scala-2.13-"
          case _ =>
            base / "scala-2.13+"
        }
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
    scalaVersion := Scala212,
    crossScalaVersions := Seq(Scala211, Scala212, Scala213, Scala3),
    commands += Command.command("setVersionUseDynver") { state =>
      val extracted = Project extract state
      val out = extracted get dynverGitDescribeOutput
      val date = extracted get dynverCurrentDate
      s"""set ThisBuild / version := "${out.sonatypeVersion(date)}" """ :: state
    },
    resolvers ++= (if (scalaVersion.value.endsWith("-SNAPSHOT")) List(Opts.resolver.sonatypeSnapshots) else Nil),
    fullResolvers ~= {_.filterNot(_.name == "jcenter")}, // https://github.com/sbt/sbt/issues/2217
    scalaCheckVersion_1_15 := {
      scalaBinaryVersion.value match {
        case "3" =>
          "1.15.4"
        case _ =>
          "1.15.2"
      }
    },
    scalaCheckGroupId := "org.scalacheck",
    scalacOptions ++= Seq(
      // contains -language:postfixOps (because 1+ as a parameter to a higher-order function is treated as a postfix op)
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps",
      "-unchecked"
    ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2,10)) => scalac210Options
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
        val dir = name.value match {
          case ConcurrentName =>
            (Compile / scalaSource).value
          case _ =>
            ScalazCrossType.shared(baseDirectory.value, "main")
        }
        typeclassSource(tc).sources.map(_.createOrUpdate(dir, s.log))
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

    (Compile / showDoc) := {
      val _ = (Compile / doc).value
      val out = (Compile / doc / target).value
      val index = out / "index.html"
      if (index.exists()) Desktop.getDesktop.open(out / "index.html")
    },

    credentialsSetting,
    publishTo := sonatypePublishToBundle.value,
    sonatypeBundleDirectory := {
      (LocalRootProject / target).value / "sonatype-staging" / (ThisBuild / version).value
    },
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
      releaseStepCommandAndRemaining("set ThisBuild / useSuperShell := false"),
      publishSignedArtifacts,
      releaseStepCommandAndRemaining(s"+ ${rootNativeId}/publishSigned"),
      releaseStepCommandAndRemaining("set ThisBuild / useSuperShell := true"),
      releaseStepCommandAndRemaining("sonatypeBundleRelease"),
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
    libraryDependencies ++= (scalaBinaryVersion.value match {
      case "2.10" =>
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch) :: Nil
      case _ =>
        Nil
    }),
    kindProjectorVersion := "0.13.2",
    libraryDependencies ++= {
      if (scalaBinaryVersion.value == "3") {
        Nil
      } else {
        Seq(compilerPlugin("org.typelevel" % "kind-projector" % kindProjectorVersion.value cross CrossVersion.full))
      }
    }
  ) ++ Seq(packageBin, packageDoc, packageSrc).flatMap {
    // include LICENSE.txt in all packaged artifacts
    inTask(_)(Seq((Compile / mappings) += licenseFile.value -> "LICENSE"))
  } ++ SbtOsgi.projectSettings ++ Seq[Sett](
    OsgiKeys.additionalHeaders := Map("-removeheaders" -> "Include-Resource,Private-Package")
  ) ++ Def.settings(
    ThisBuild / mimaReportSignatureProblems := true,
    mimaBinaryIssueFilters ++= {
      if (scalaBinaryVersion.value == "2.11") {
        Seq(
          ProblemFilters.exclude[IncompatibleSignatureProblem]("scalaz.*"),
          ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("scalaz.Isomorphisms#IsoFunctorTemplate.to_"),
          ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("scalaz.Isomorphisms#IsoFunctorTemplate.from_"),
          ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("scalaz.Isomorphisms#IsoBifunctorTemplate.to_"),
          ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("scalaz.Isomorphisms#IsoBifunctorTemplate.from_"),
        )
      } else {
        Nil
      }
    },
    mimaPreviousArtifacts := {
      if ((scalaBinaryVersion.value == "3") && (scalazMimaBasis.?.value == Some("7.2.33"))) {
        Set.empty
      } else {
        scalazMimaBasis.?.value.map {
          organization.value % s"${name.value}_${scalaBinaryVersion.value}" % _
        }.toSet
      }
    }
  )

  val nativeSettings = Seq(
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
      if ((scalaBinaryVersion.value == "3") && (scalazMimaBasis.?.value == Some("7.2.33"))) {
        Set.empty
      } else {
        scalazMimaBasis.?.value.map {
          organization.value % s"${name.value}_native0.4_${scalaBinaryVersion.value}" % _
        }.toSet
      }
    }
  )

  lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(ScalazCrossType)
    .settings(standardSettings)
    .settings(
      mimaBinaryIssueFilters ++= {
        import com.typesafe.tools.mima.core._
        import com.typesafe.tools.mima.core.ProblemFilters._
        Nil
      },
      name := "scalaz-core",
      (Compile / sourceGenerators) += (Compile / sourceManaged).map{
        dir => Seq(GenerateTupleW(dir), TupleNInstances(dir))
      }.taskValue,
      buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
      buildInfoPackage := buildInfoPackageName,
      osgiExport("scalaz"),
      OsgiKeys.importPackage := Seq("javax.swing;resolution:=optional", "*"))
    .enablePlugins(sbtbuildinfo.BuildInfoPlugin, MimaPlugin)
    .jsSettings(scalajsProjectSettings)
    .jvmSettings(
      typeClasses := TypeClass.core
    )
    .nativeSettings(
      nativeSettings
    )

  final val ConcurrentName = "scalaz-concurrent"

  lazy val effect = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(ScalazCrossType)
    .settings(standardSettings)
    .settings(
      name := "scalaz-effect",
      osgiExport("scalaz.effect", "scalaz.std.effect", "scalaz.syntax.effect"))
    .dependsOn(core)
    .enablePlugins(MimaPlugin)
    .jsSettings(scalajsProjectSettings)
    .jvmSettings(
      typeClasses := TypeClass.effect
    )
    .nativeSettings(
      nativeSettings
    )

  lazy val iteratee = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(ScalazCrossType)
    .settings(standardSettings)
    .settings(
      name := "scalaz-iteratee",
      osgiExport("scalaz.iteratee"))
    .dependsOn(core, effect)
    .enablePlugins(MimaPlugin)
    .jsSettings(scalajsProjectSettings)
    .nativeSettings(
      nativeSettings
    )

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

  lazy val genTypeClasses = taskKey[Seq[(FileStatus, File)]]("")

  lazy val typeClasses = taskKey[Seq[TypeClass]]("")

  lazy val genToSyntax = taskKey[String]("")

  lazy val showDoc = taskKey[Unit]("")

  lazy val typeClassTree = taskKey[String]("Generates scaladoc formatted tree of type classes.")

  lazy val checkGenTypeClasses = taskKey[Unit]("")

  def osgiExport(packs: String*) = OsgiKeys.exportPackage := packs.map(_ + ".*;version=${Bundle-Version}")
}

// vim: expandtab:ts=2:sw=2
