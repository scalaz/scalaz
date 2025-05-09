import GenTypeClass.*
import com.jsuereth.sbtpgp.SbtPgp.autoImport.PgpKeys.publishLocalSigned
import com.jsuereth.sbtpgp.SbtPgp.autoImport.PgpKeys.publishSigned
import java.awt.Desktop
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbt.*
import sbt.Keys.*
import sbtbuildinfo.BuildInfoPlugin.autoImport.*
import sbtcrossproject.CrossPlugin.autoImport.*
import sbtrelease.ReleasePlugin.autoImport.*
import sbtrelease.ReleaseStateTransformations.*
import sbtrelease.Utilities.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*
import scalanativecrossproject.ScalaNativeCrossPlugin.autoImport.*
import xerial.sbt.Sonatype.autoImport.*

object build {
  type Sett = Def.Setting[_]

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

  val kindProjectorVersion = SettingKey[String]("kindProjectorVersion")

  private[this] def gitHash(): String = sys.process.Process("git rev-parse HEAD").lineStream_!.head

  private[this] val tagName = Def.setting{
    s"v${if (releaseUseGlobalVersion.value) (ThisBuild / version).value else version.value}"
  }
  private[this] val tagOrHash = Def.setting{
    if(isSnapshot.value) gitHash() else tagName.value
  }

  private val wasmSetting: Def.SettingsDefinition = (
    if (sys.props.isDefinedAt("scala_js_wasm")) {
      println("enable wasm")
      Def.settings(
        scalaJSLinkerConfig ~= (
          _.withExperimentalUseWebAssembly(true).withModuleKind(ModuleKind.ESModule)
        ),
        jsEnv := {
          import org.scalajs.jsenv.nodejs.NodeJSEnv
          val config = NodeJSEnv.Config()
            .withArgs(List(
              "--experimental-wasm-exnref",
              "--experimental-wasm-imported-strings",
              "--turboshaft-wasm",
            ))
          new NodeJSEnv(config)
        },
      )
    } else {
      Def.settings()
    }
  )

  val scalajsProjectSettings = Def.settings(
    wasmSetting,
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
        case NativePlatform => "native"
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

  private def Scala213 = "2.13.16"
  private def Scala3 = "3.3.6"

  private[this] val buildInfoPackageName = "scalaz"

  lazy val unmanagedSourcePathSettings: Seq[Sett] = Def.settings(
    Seq(Compile, Test).map { scope =>
      (scope / unmanagedSourceDirectories) ++= {
        val dir = Defaults.nameForSrc(scope.name)
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
  )

  lazy val standardSettings: Seq[Sett] = Def.settings(
    organization := "org.scalaz",
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
    commands += Command.command("SetScala3") {
      s"""++ ${Scala3}! -v""" :: _
    },
    scalaVersion := Scala213,
    crossScalaVersions := Seq(Scala213, Scala3),
    fullResolvers ~= {_.filterNot(_.name == "jcenter")}, // https://github.com/sbt/sbt/issues/2217
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked"
    ),
    Compile / scalacOptions ++= {
      scalaBinaryVersion.value match {
        case "2.13" =>
          Seq("-Wperformance")
        case _ =>
          Nil
      }
    },
    scalacOptions ++= {
      val common = "implicitConversions,higherKinds,existentials"
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((0 | 3, _)) =>
          Seq(
            "-Ykind-projector",
            s"-language:$common",
          )
        case _ =>
          Seq(
            "-Xsource:3-cross",
            "-Xlint:_,-type-parameter-shadow,-missing-interpolator",
            "-Ywarn-dead-code",
            "-Ywarn-numeric-widen",
            "-Ywarn-value-discard",
            "-Xlint:adapted-args",
            s"-language:$common",
          ) ++ unusedWarnOptions.value
      }
    },
    Seq(Compile, Test).flatMap(c =>
      (c / console / scalacOptions) --= unusedWarnOptions.value
    ),

    (Compile / doc / scalacOptions) := {
      val tag = tagOrHash.value
      val base = (LocalRootProject / baseDirectory).value.getAbsolutePath
      val options = (Compile / doc / scalacOptions).value

      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Nil
        case _ =>
          options ++ Seq("-sourcepath", base, "-doc-source-url", "https://github.com/scalaz/scalaz/tree/" + tag + "€{FILE_PATH}.scala")
      }
    },
    (Compile / doc / sources) := {
      val src = (Compile / doc / sources).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Nil
        case _ =>
          src
      }
    },

    // retronym: I was seeing intermittent heap exhaustion in scalacheck based tests, so opting for determinism.
    Test / parallelExecution := false,
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
      val LICENSE_txt = (ThisBuild / baseDirectory).value / "LICENSE.txt"
      if (!LICENSE_txt.exists()) sys.error(s"cannot find license file at $LICENSE_txt")
      LICENSE_txt
    },
    // kind-projector plugin
    kindProjectorVersion := "0.13.3",
    libraryDependencies ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
      case Some((2, _)) =>
        Seq(
          compilerPlugin("org.typelevel" % "kind-projector" % kindProjectorVersion.value cross CrossVersion.full)
        )
    }.toList.flatten
  ) ++ Seq(packageBin, packageDoc, packageSrc).flatMap {
    // include LICENSE.txt in all packaged artifacts
    inTask(_)(Seq((Compile / mappings) += licenseFile.value -> "LICENSE"))
  }

  private[this] val jvm_js_settings = Seq(
    (Compile / unmanagedSourceDirectories) += {
      baseDirectory.value.getParentFile / "jvm_js/src/main/scala/"
    }
  )

  lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(ScalazCrossType)
    .settings(standardSettings: _*)
    .settings(
      unmanagedSourcePathSettings,
      name := "scalaz-core",
      Compile / sourceGenerators += (Compile / sourceManaged).map{
        dir => Seq(GenerateTupleW(dir), TupleNInstances(dir))
      }.taskValue,
      buildInfoKeys := Seq[BuildInfoKey](version, scalaVersion),
      buildInfoPackage := buildInfoPackageName,
      buildInfoObject := "ScalazBuildInfo",
    )
    .enablePlugins(sbtbuildinfo.BuildInfoPlugin)
    .jsSettings(
      jvm_js_settings,
      scalajsProjectSettings,
      libraryDependencies += ("org.scala-js" %%% "scalajs-weakreferences" % "1.0.0" % Optional).cross(CrossVersion.for3Use2_13)
    )
    .jvmSettings(
      jvm_js_settings,
      typeClasses := TypeClass.core
    )

  lazy val effect = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(ScalazCrossType)
    .settings(standardSettings: _*)
    .settings(
      unmanagedSourcePathSettings,
      name := "scalaz-effect",
    )
    .dependsOn(core)
    .jsSettings(scalajsProjectSettings : _*)
    .jvmSettings(
      typeClasses := TypeClass.effect
    )

  lazy val iteratee = crossProject(JSPlatform, JVMPlatform, NativePlatform).crossType(ScalazCrossType)
    .settings(standardSettings: _*)
    .settings(
      unmanagedSourcePathSettings,
      name := "scalaz-iteratee",
    )
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

}

// vim: expandtab:ts=2:sw=2
