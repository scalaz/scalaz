import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._
import com.jsuereth.pgp.sbtplugin.PgpPlugin._


object ScalazBuild extends Build {
  lazy val scalaz = Project(
    id        = "scalaz",
    base      = file("."),
    settings  = standardSettings,
    aggregate = Seq(core, http, geo, example, scalacheckBinding, scalacheckGeo, tests, full)
  )

  lazy val core = Project(
    id       = "scalaz-core",
    base     = file("core"),
    settings = standardSettings ++ Seq(
      (sourceGenerators in Compile) <+= (sourceManaged in Compile) map {
        dir => Seq(Boilerplate.generateTupleW(dir))
      }
    )
  )

  lazy val geo = Project(
    id           = "scalaz-geo",
    base         = file("geo"),
    dependencies = Seq(core),
    settings     = standardSettings
  )

  lazy val http = Project(
    id           = "scalaz-http",
    base         = file("http"),
    dependencies = Seq(core),
    settings     = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependency.ServletApi)
    )
  )

  lazy val scalacheckBinding = Project(
    id           = "scalaz-scalacheck-binding",
    base         = file("scalacheck-binding"),
    dependencies = Seq(core),
    settings     = standardSettings ++ Seq(
      libraryDependencies <++= (dependencyScalaVersion)(dsv => Seq(Dependency.ScalaCheck(dsv)))
    )
  )

  lazy val scalacheckGeo = Project(
    id           = "scalaz-geo-scalacheck",
    base         = file("geo-scalacheck"),
    dependencies = Seq(core, geo, scalacheckBinding),
    settings     = standardSettings ++ Seq(
      libraryDependencies <++= (dependencyScalaVersion)(dsv => Seq(Dependency.ScalaCheck(dsv)))
    )
  )

  lazy val example = Project(
    id           = "scalaz-example",
    base         = file("example"),
    dependencies = Seq(core, geo, http),
    settings     = standardSettings ++ Seq(
      libraryDependencies <++= (dependencyScalaVersion)(dsv => Seq(Dependency.Specs(dsv), Dependency.ServletApi))
    )
  )

  lazy val tests = Project(
    id           = "scalaz-test-suite",
    base         = file("tests"),
    dependencies = Seq(core, geo, scalacheckBinding, scalacheckGeo),
    settings     = standardSettings ++ Seq(
      libraryDependencies <++= (dependencyScalaVersion)(dsv => Seq(Dependency.Specs(dsv)))
    )
  )

  lazy val full = {
    // The projects that are packaged in the full distribution.
    val projects = Seq(core, scalacheckBinding, geo, scalacheckGeo, http, example)

    // Some intermediate keys to simplify extracting a task or setting from `projects`.
    val allPackagedArtifacts = TaskKey[Seq[Map[Artifact, File]]]("all-packaged-artifacts")
    val allSources           = TaskKey[Seq[Seq[File]]]("all-sources")
    val allSourceDirectories = SettingKey[Seq[Seq[File]]]("all-source-directories")

    def artifactMappings(rootBaseDir: File, baseDir: File, scalaVersion: String, version: String,
                         fullDocDir: File, artifacts: Seq[Map[Artifact, File]]): Seq[(File, String)] = {
      val sxrDocDirectory = new File(fullDocDir.getAbsolutePath + ".sxr")

      // Include a root folder in the generated archive.
      val newBase = "scalaz_%s-%s".format(scalaVersion, version)

      val jarsAndPomMappings = artifacts.flatMap(_.values) x flatRebase(newBase)
      val etcMappings        = ((rootBaseDir / "etc" ** "*") +++ Seq(rootBaseDir / "README")) x rebase(rootBaseDir, newBase)
      val fullDocMappings    = (fullDocDir ** "*") x rebase(fullDocDir.getParentFile, newBase)
      val sxrDocMappings     = (sxrDocDirectory ** "*") x rebase(sxrDocDirectory.getParentFile, newBase)
      jarsAndPomMappings ++ etcMappings ++ fullDocMappings ++ sxrDocMappings
    }

    /** Scalac options for SXR */
    def sxrOptions(baseDir: File, sourceDirs: Seq[Seq[File]]): Seq[String] = {
      val xplugin = "-Xplugin:" + (baseDir / "lib" / "sxr_2.9.0-0.2.7.jar").asFile.getAbsolutePath
      val baseDirs = sourceDirs.flatten
      val sxrBaseDir = "-P:sxr:base-directory:" + baseDirs.mkString(":")
      Seq(xplugin, sxrBaseDir)
    }

    Project(
      id           = "scalaz-full",
      base         = file("full"),
      dependencies = Seq(core, scalacheckBinding, http, example, tests),
      settings     = standardSettings ++ Seq(
        allSources           <<= projects.map(sources in Compile in _).join, // join: Seq[Task[A]] => Task[Seq[A]]
        allSourceDirectories <<= projects.map(sourceDirectories in Compile in _).join,
        allPackagedArtifacts <<= projects.map(packagedArtifacts in _).join,

        // Combine the sources of other modules to generate Scaladoc and SXR annotated sources
        (sources in Compile) <<= (allSources).map(_.flatten),

        // Avoid compiling the sources here; we just are after scaladoc.
        (compile in Compile) := inc.Analysis.Empty,

        // Include SXR in the Scaladoc Build to generated HTML annotated sources.
        scalacOptions in (Compile, doc) <++= (baseDirectory, allSourceDirectories, scalaVersion, version, baseDirectory in LocalProject("scalaz")).map {
          (bd, asd, sv, v, rootBase) =>
            // No idea why these get lost from standardSettings and I need to duplicate them here.
            val tagOrBranch = if (v.endsWith("-SNAPSHOT")) "master" else "v" + v
            val docSourceUrl = "https://github.com/scalaz/scalaz/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
            val docsourceOpts = Seq("-sourcepath", rootBase.getAbsolutePath, "-doc-source-url", docSourceUrl)

            val sxrOpts = if (sv.startsWith("2.10") || sv.startsWith("2.8")) Seq() else sxrOptions(bd, asd)
            docsourceOpts ++ sxrOpts

        },

        // Package an archive containing all artifacts, readme, licence, and documentation.
        // Use `LocalProject("scalaz")` rather than `scalaz` to avoid a circular reference.
        (mappings in packageBin in Compile) <<= (
                baseDirectory in LocalProject("scalaz"), baseDirectory, scalaVersion, version,
                target in doc in Compile, allPackagedArtifacts) map artifactMappings
      )
    )
  }

  object Dependency {
    // SBT's built in '%%' is not flexible enough. When we build with a snapshot version of the compiler,
    // we want to fetch dependencies from the last stable release (hopefully binary compatibility).
    def dependencyScalaVersion(currentScalaVersion: String): String = currentScalaVersion match {
      case "2.10.0-SNAPSHOT" | "2.10.0-M4" => "2.9.2"
      case x => x
    }
    val ServletApi = "javax.servlet" % "servlet-api" % "2.5"

    def ScalaCheck(scalaVersion: String) = {
      val version = scalaVersion match {
        case "2.8.1" => "1.8"
        case _ => "1.9"
      }
      "org.scalacheck" % "scalacheck_%s".format(scalaVersion) % version
    }
    def Specs(scalaVersion: String) = {
      val version = scalaVersion match {
        case "2.8.1" | "2.8.2" | "2.9.0-1" => "1.6.8"
        case _ => "1.6.9"
      }
      "org.scala-tools.testing" % "specs_%s".format(scalaVersion) % version % "test"
    }
  }

  val dependencyScalaVersionTranslator = SettingKey[(String => String)]("dependency-scala-version-translator", "Function to translate the current scala version to the version used for dependency resolution")
  val dependencyScalaVersion = SettingKey[String]("dependency-scala-version", "The version of scala appended to module id of dependencies")

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalaz",
    version      := "6.0.5-SNAPSHOT",
    scalaVersion := "2.9.2",
    crossScalaVersions := Seq("2.9.2", "2.9.1", "2.9.0-1", "2.8.1", "2.10.0-M4"),
    crossVersion := CrossVersion.full,
    resolvers    ++= Seq(
      "snapshotsResolver" at "http://oss.sonatype.org/content/repositories/snapshots",
      "releasesResolver"  at "http://oss.sonatype.org/content/repositories/releases"
    ),

    dependencyScalaVersionTranslator := (Dependency.dependencyScalaVersion _),
    dependencyScalaVersion           <<= (dependencyScalaVersionTranslator, scalaVersion)((t, sv) => t(sv)),
    publishSetting,
    scalacOptions in(Compile, doc) <++= (version, baseDirectory in LocalProject("scalaz")).map {
      (v, bd) =>
        val tagOrBranch = if (v.endsWith("-SNAPSHOT")) "master" else "v" + v
        val docSourceUrl = "https://github.com/scalaz/scalaz/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
        Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", docSourceUrl)
    },

    // TODO remove after deprecating Scala 2.9.0.1
    (unmanagedClasspath in Compile) += Attributed.blank(file("dummy")),

    credentialsSetting,
    scalacOptions  ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
    packageOptions ++= Seq[PackageOption](ManifestAttributes(
      (IMPLEMENTATION_TITLE,  "Scalaz"),
      (IMPLEMENTATION_URL,    "http://code.google.com/p/scalaz"),
      (IMPLEMENTATION_VENDOR, "The Scalaz Project"),
      (SEALED, "true"))
    ),
    useGpg := true,
    useGpgAgent := false,
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
            ("nuttycom", "Kris Nuttycombe")
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
}
