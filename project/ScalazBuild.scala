import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._


object ScalazBuild extends Build {
  lazy val scalaz = Project(
    id = "scalaz",
    base = file("."),
    settings = standardSettings,
    aggregate = Seq(core, http, geo, example, scalacheckBinding, scalacheckGeo, tests, full)
  )

  lazy val core = Project(
    id = "scalaz-core",
    base = file("core"),
    settings = standardSettings ++ Seq(
      (sourceGenerators in Compile) <+= (sourceManaged in Compile) map {
        dir => Seq(Boilerplate.generateTupleW(dir))
      }
    )
  )

  lazy val geo = Project(
    id = "scalaz-geo",
    base = file("geo"),
    dependencies = Seq(core),
    settings = standardSettings
  )

  lazy val http = Project(
    id = "scalaz-http",
    base = file("http"),
    dependencies = Seq(core),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependency.ServletApi)
    )
  )

  lazy val scalacheckBinding = Project(
    id = "scalaz-scalacheck-binding",
    base = file("scalacheck-binding"),
    dependencies = Seq(core),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependency.ScalaCheck)
    )
  )

  lazy val scalacheckGeo = Project(
    id = "scalaz-geo-scalacheck",
    base = file("geo-scalacheck"),
    dependencies = Seq(core, geo, scalacheckBinding),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependency.ScalaCheck)
    )
  )

  lazy val example = Project(
    id = "scalaz-example",
    base = file("example"),
    dependencies = Seq(core, geo, http),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependency.Specs, Dependency.ServletApi)
    )
  )

  lazy val tests = Project(
    id = "scalaz-test-suite",
    base = file("tests"),
    dependencies = Seq(core, geo, scalacheckBinding, scalacheckGeo),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependency.Specs)
    )
  )

  lazy val full = {
    // The projects that are packaged in the full distribution.
    val projects = Seq(core, scalacheckBinding, geo, scalacheckGeo, http, example)

    // Some intermediate keys to simplify extracting a task or setting from `projects`.
    val allPackagedArtifacts = TaskKey[Seq[Map[Artifact, File]]]("all-packaged-artifacts")
    val allSources = TaskKey[Seq[Seq[File]]]("all-sources")
    val allSourceDirectories = SettingKey[Seq[Seq[File]]]("all-source-directories")

    def artifactMappings(rootBaseDir: File, baseDir: File, scalaVersion: String, version: String,
                         fullDocDir: File, artifacts: Seq[Map[Artifact, File]]): Seq[(File, String)] = {
      val sxrDocDirectory = new File(fullDocDir.getAbsolutePath + ".sxr")

      // Include a root folder in the generated archive.
      val newBase = "scalaz_%s-%s".format(scalaVersion, version)

      val jarsAndPomMappings = artifacts.flatMap(_.values) x flatRebase(newBase)
      val etcMappings = ((rootBaseDir / "etc" ** "*") +++ Seq(rootBaseDir / "README")) x rebase(rootBaseDir, newBase)
      val fullDocMappings = (fullDocDir ** "*") x rebase(fullDocDir.getParentFile, newBase)
      val sxrDocMappings = (sxrDocDirectory ** "*") x rebase(sxrDocDirectory.getParentFile, newBase)
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
      id = "scalaz-full",
      base = file("full"),
      dependencies = Seq(core, scalacheckBinding, http, example, tests),
      settings = standardSettings ++ Seq(
        allSources <<= projects.map(sources in Compile in _).join, // join: Seq[Task[A]] => Task[Seq[A]]
        allSourceDirectories <<= projects.map(sourceDirectories in Compile in _).join,
        allPackagedArtifacts <<= projects.map(packagedArtifacts in _).join,

        // Combine the sources of other modules to generate Scaladoc and SXR annotated sources
        (sources in Compile) <<= (allSources).map(_.flatten),

        // Avoid compiling the sources here; we just are after scaladoc.
        (compile in Compile) := inc.Analysis.Empty,

        // Include SXR in the Scaladoc Build to generated HTML annotated sources.
        (scaladocOptions in Compile) <++= (baseDirectory, allSourceDirectories) map sxrOptions,

        // Package an archive containing all artifacts, readme, licence, and documentation.
        // Use `LocalProject("scalaz")` rather than `scalaz` to avoid a circular reference.
        (mappings in packageBin in Compile) <<= (
                baseDirectory in LocalProject("scalaz"), baseDirectory, scalaVersion, version,
                docDirectory in Compile, allPackagedArtifacts) map artifactMappings
      )
    )
  }

  object Dependency {
    val ServletApi = "javax.servlet" % "servlet-api" % "2.5"
    val ScalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.9"
    val Specs = "org.scala-tools.testing" %% "specs" % "1.6.8" % "test"
  }

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalaz",
    version := "6.0.2-SNAPSHOT",
    scalaVersion := "2.9.0-1",
    publishSetting,
    credentialsSetting,
    scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
    packageOptions ++= Seq[PackageOption](ManifestAttributes(
      (IMPLEMENTATION_TITLE, "Scalaz"),
      (IMPLEMENTATION_URL, "http://code.google.com/p/scalaz"),
      (IMPLEMENTATION_VENDOR, "The Scalaz Project"),
      (SEALED, "true"))
    )
  )

  lazy val publishSetting = publishTo <<= (version) {
    version: String =>
      def repo(name: String) = name at "http://nexus-direct.scala-tools.org/content/repositories/" + name
      val isSnapshot = version.trim.endsWith("SNAPSHOT")
      val repoName = if(isSnapshot) "snapshots" else "releases"
      Some(repo(repoName))
  }

  lazy val credentialsSetting = credentials += {
    Seq("build.publish.user", "build.publish.password").map(k => Option(System.getProperty(k))) match {
      case Seq(Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "nexus.scala-tools.org", user, pass)
      case _ =>
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }
  }
}
