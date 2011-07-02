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
    Project("scalaz-full",
      file("full"),
      settings = standardSettings ++ Seq(
        // Combine the sources of other modules to generate Scaladoc and SXR annotated sources
        (sources in Compile) <<= (
                sources in core in Compile,
                sources in geo in Compile,
                sources in scalacheckBinding in Compile,
                sources in example in Compile).map(_ ++ _ ++ _ ++ _),
        // don't recompile the sources
        compile := inc.Analysis.Empty,
        (scaladocOptions in Compile) <++= (baseDirectory,
                sourceDirectories in core in Compile,
                sourceDirectories in scalacheckBinding in Compile,
                sourceDirectories in geo in Compile, // TODO why does SXR put Azimuth.html in the root dir?
                sourceDirectories in scalacheckGeo in Compile,
                sourceDirectories in http in Compile,
                sourceDirectories in example in Compile) map {
          (bd, d0, d1, d2, d3, d4, d5) =>
            val xplugin = "-Xplugin:" + (bd / "lib" / "sxr_2.8.0.RC2-0.2.4-SNAPSHOT.jar").asFile.getAbsolutePath
            val baseDirs = Seq(d0, d2, d2, d3, d4, d5).flatten
            val sxrBaseDir = "-P:sxr:base-directory:" + baseDirs.mkString(":")
            Seq(xplugin, sxrBaseDir)
        },
        (mappings in packageBin in Compile) <<= (
                baseDirectory,
                docDirectory in Compile,
                packagedArtifacts in core,
                packagedArtifacts in scalacheckBinding,
                packagedArtifacts in geo,
                packagedArtifacts in scalacheckGeo,
                packagedArtifacts in http,
                packagedArtifacts in example) map {
          (bd, fullDocDir, a0, a1, a2, a3, a4, a5) =>
            val sxrDocDirectory = new File(fullDocDir.getAbsolutePath + ".sxr")

            // a bit hacky, but we can't access `baseDirectory in scalaz` without a circular reference
            val rootDir = bd.getParentFile

            val jarsAndPomMappings = Seq(a0, a1, a2, a3, a4, a5).flatMap(_.values) x flat
            val etcMappings = ((rootDir / "etc" ** "*") +++ Seq(rootDir / "README")) x relativeTo(rootDir)
            val fullDocMappings = (fullDocDir ** "*") x relativeTo(fullDocDir.getParentFile)
            val sxrDocMappings = (sxrDocDirectory ** "*") x relativeTo(sxrDocDirectory.getParentFile)
            val allMappings = jarsAndPomMappings ++ etcMappings ++ fullDocMappings ++ sxrDocMappings
            allMappings.map {
              case (input, output) =>
                (input, "scalaz/" + output) // TODO Include Scala and Scalaz versions in this directory name.
            }
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
    credentials += {
      // TODO first look up properties "build.publish.{user, password}" for CI build.
      Credentials(Path.userHome / ".ivy2" / ".credentials")
    },
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
}
