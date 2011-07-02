import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._


object ScalazBuild extends Build {
  lazy val scalaz = Project("scalaz",
    file("."),
    settings = standardSettings
  ) aggregate (core, http, geo, example, scalacheckBinding, scalacheckGeo, tests, full)

  lazy val core = Project("scalaz-core",
    file("core"),
    settings = standardSettings ++ Seq(
      (sourceGenerators in Compile) <+= (sourceManaged in Compile) map {
        dir => Seq(Boilerplate.generateTupleW(dir))
      }
    )
  )

  lazy val geo = Project("scalaz-geo",
    file("geo"),
    settings = standardSettings
  ) dependsOn (core)

  lazy val http = Project("scalaz-http",
    file("http"),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependency.ServletApi)
    )
  ) dependsOn (core)

  lazy val scalacheckBinding = Project("scalaz-scalacheck-binding",
    file("scalacheck-binding"),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependency.ScalaCheck)
    )
  ) dependsOn (core)

  lazy val scalacheckGeo = Project("scalaz-geo-scalacheck",
    file("geo-scalacheck"),
    settings = standardSettings
  ) dependsOn (geo, scalacheckBinding)

  lazy val example = Project("scalaz-example",
    file("example"),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependency.Specs, Dependency.ServletApi)
    )
  ) dependsOn (core, geo, http)

  lazy val tests = Project("scalaz-test-suite",
    file("tests"),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependency.Specs)
    )
  ) dependsOn (core, geo, scalacheckBinding, scalacheckGeo)

  lazy val full = {
    // The projects that are packaged in the full distribution.
    val projects = Seq(core, scalacheckBinding, geo, scalacheckGeo, http, example)

    // Some intermediate keys to simplify extracting a task or setting from `projects`.
    val allPackagedArtifacts = TaskKey[Seq[Map[Artifact, File]]]("all-packaged-artifacts")
    val allSources = TaskKey[Seq[Seq[File]]]("all-sources")
    val allSourceDirectories = SettingKey[Seq[Seq[File]]]("all-source-directories")

    Project("scalaz-full",
      file("full"),
      settings = standardSettings ++ Seq(
        allSources <<= projects.map(sources in Compile in _).join, // join: Seq[Task[A]] => Task[Seq[A]]
        allSourceDirectories <<= projects.map(sourceDirectories in Compile in _).join,
        allPackagedArtifacts <<= projects.map(packagedArtifacts in _).join,

        // Combine the sources of other modules to generate Scaladoc and SXR annotated sources
        (sources in Compile) <<= (allSources).map(_.flatten),

        // Avoid compiling the sources here; we just are after scaladoc.
        (compile in Compile) := inc.Analysis.Empty,

        // Include SXR in the Scaladoc Build to generated HTML annotated sources.
        (scaladocOptions in Compile) <++= (baseDirectory, allSourceDirectories) map {
          (bd, ds) =>
            val xplugin = "-Xplugin:" + (bd / "lib" / "sxr_2.8.0.RC2-0.2.4-SNAPSHOT.jar").asFile.getAbsolutePath
            val baseDirs = ds.flatten
            val sxrBaseDir = "-P:sxr:base-directory:" + baseDirs.mkString(":")
            Seq(xplugin, sxrBaseDir)
        },

        // Package an archive containing all artifacts, readme, licence, and documentation.
        // Use `LocalProject("scalaz")` rather than `scalaz` to avoid a circular reference.
        (mappings in packageBin in Compile) <<= (
                baseDirectory in LocalProject("scalaz"), baseDirectory, scalaVersion, version,
                docDirectory in Compile, allPackagedArtifacts) map {
          (rootDir, bd, sv, v, fullDocDir, artifacts) =>
            val sxrDocDirectory = new File(fullDocDir.getAbsolutePath + ".sxr")

            // Include a root folder in the generated archive.
            val newBase = "scalaz_%s-%s".format(sv, v)

            val jarsAndPomMappings = artifacts.flatMap(_.values) x flatRebase(newBase)
            val etcMappings = ((rootDir / "etc" ** "*") +++ Seq(rootDir / "README")) x rebase(rootDir, newBase)
            val fullDocMappings = (fullDocDir ** "*") x rebase(fullDocDir.getParentFile, newBase)
            val sxrDocMappings = (sxrDocDirectory ** "*") x rebase(sxrDocDirectory.getParentFile, newBase)
            jarsAndPomMappings ++ etcMappings ++ fullDocMappings ++ sxrDocMappings
        }
      )
    ) dependsOn (core, scalacheckBinding, http, example, tests)
  }

  object Dependency {
    val ServletApi = "javax.servlet" % "servlet-api" % "2.5"
    val ScalaCheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8"
    val Specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.7.2" % "test"
  }

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalaz",
    version := "6.0.2-SNAPSHOT",
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
