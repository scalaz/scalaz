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
      libraryDependencies <++= (dependencyScalaVersion)(dsv => Seq(Dependency.ScalaCheck(dsv)))
    )
  )

  lazy val scalacheckGeo = Project(
    id = "scalaz-geo-scalacheck",
    base = file("geo-scalacheck"),
    dependencies = Seq(core, geo, scalacheckBinding),
    settings = standardSettings ++ Seq(
      libraryDependencies <++= (dependencyScalaVersion)(dsv => Seq(Dependency.ScalaCheck(dsv)))
    )
  )

  lazy val example = Project(
    id = "scalaz-example",
    base = file("example"),
    dependencies = Seq(core, geo, http),
    settings = standardSettings ++ Seq(
      libraryDependencies <++= (dependencyScalaVersion)(dsv => Seq(Dependency.Specs(dsv), Dependency.ServletApi))
    )
  )

  lazy val tests = Project(
    id = "scalaz-test-suite",
    base = file("tests"),
    dependencies = Seq(core, geo, scalacheckBinding, scalacheckGeo),
    settings = standardSettings ++ Seq(
      libraryDependencies <++= (dependencyScalaVersion)(dsv => Seq(Dependency.Specs(dsv)))
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
      val xplugin = "-Xplugin:" + (baseDir / "lib" / "sxr_2.8.0.RC2-0.2.4-SNAPSHOT.jar").asFile.getAbsolutePath
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
    // SBT's built in '%%' is not flexible enough. When we build with a snapshot version of the compiler,
    // we want to fetch dependencies from the last stable release (hopefully binary compatibile).
    def dependencyScalaVersion(currentScalaVersion: String): String = currentScalaVersion match {
      case "2.10.0-SNAPSHOT" => "2.9.0-1"
      case x => x
    }
    val ServletApi = "javax.servlet" % "servlet-api" % "2.5"

    def ScalaCheck(scalaVersion: String) = "org.scala-tools.testing" % "scalacheck_%s".format(scalaVersion) % "1.8"
    def Specs(scalaVersion: String) = "org.scala-tools.testing" % "specs_%s".format(scalaVersion) % "1.6.8" % "test"
  }

  val dependencyScalaVersionTranslator = SettingKey[(String => String)]("dependency-scala-version-translator", "Function to translate the current scala version to the version used for dependency resolution")
  val dependencyScalaVersion = SettingKey[String]("dependency-scala-version", "The version of scala appended to module id of dependencies")

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalaz",
    version := "6.0.2-SNAPSHOT",
    scalaVersion := "2.8.1",
    resolvers += ScalaToolsSnapshots,  
    dependencyScalaVersionTranslator := (Dependency.dependencyScalaVersion _),
    dependencyScalaVersion <<= (dependencyScalaVersionTranslator, scalaVersion)((t, sv) => t(sv)),
    publishSetting,

    // TODO remove after updating to SBT 0.10.1, https://github.com/harrah/xsbt/commit/520f74d1146a1ba6244187c52a951eb4d0f9cc8c
    (unmanagedClasspath in Compile) += Attributed.blank(file("dummy")),

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
        Credentials("Sonatype Nexus Repository Manager", "nexus-direct.scala-tools.org", user, pass)
      case _ =>
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }
  }
}
