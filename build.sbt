// shadow sbt-scalajs' crossProject from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import Scalaz._

cancelable in Global := true

organization in ThisBuild := "org.scalaz"

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

dynverSonatypeSnapshots in ThisBuild := true

lazy val sonataCredentials = for {
  username <- sys.env.get("SONATYPE_USERNAME")
  password <- sys.env.get("SONATYPE_PASSWORD")
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)

credentials in ThisBuild ++= sonataCredentials.toSeq

findLicense

lazy val root = project
  .in(file("."))
  .settings(
    skip in publish := true
  )
  .aggregate(baseJVM, baseJS, lawsJVM, lawsJS, tests, metaJVM, metaJS, microsite, benchmarks)
  .enablePlugins(ScalaJSPlugin)

def zioUri      = uri("git://github.com/scalaz/scalaz-zio.git#d315153469") // 2018/08/09
lazy val zioJVM = ProjectRef(zioUri, "coreJVM")
lazy val zioJS  = ProjectRef(zioUri, "coreJS")

lazy val base = crossProject(JSPlatform, JVMPlatform)
  .in(file("base"))
  .settings(stdSettings("base"))
  .dependsOn(meta)
  .jvmConfigure(_ dependsOn zioJVM)
  .jsConfigure(_ dependsOn zioJS)

lazy val baseJVM = base.jvm

lazy val baseJS = base.js

lazy val benchmarks = project.module
  .dependsOn(baseJVM)
  .enablePlugins(JmhPlugin)
  .settings(
    skip in publish := true,
    libraryDependencies ++=
      Seq(
        "org.scala-lang" % "scala-reflect"  % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
        "org.scalaz"     %% "scalaz-core"   % "7.2.23",
      )
  )

lazy val meta = crossProject(JSPlatform, JVMPlatform)
  .in(file("meta"))
  .settings(stdSettings("meta"))
  .settings(
    libraryDependencies ++=
      Seq("org.scala-lang" % "scala-reflect"  % scalaVersion.value,
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided")
  )

lazy val metaJVM = meta.jvm

lazy val metaJS = meta.js

lazy val microsite = project.module
  .dependsOn(baseJVM)
  .enablePlugins(MicrositesPlugin, BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "scalaz",
    buildInfoObject := "BuildInfo"
  )
  .settings(
    scalacOptions -= "-Yno-imports",
    scalacOptions ~= { _ filterNot (_ startsWith "-Ywarn") },
    scalacOptions ~= { _ filterNot (_ startsWith "-Xlint") },
    skip in publish := true,
    libraryDependencies += "com.github.ghik" %% "silencer-lib" % "1.0",
    micrositeFooterText := Some("""
                                  |<p>&copy; 2018 <a href="https://github.com/scalaz/scalaz">Scalaz Maintainers</a></p>
                                  |""".stripMargin),
    micrositeName := "Scalaz",
    micrositeDescription := "Scalaz",
    micrositeAuthor := "Scalaz contributors",
    micrositeOrganizationHomepage := "https://github.com/scalaz/scalaz",
    micrositeGitterChannelUrl := "scalaz/scalaz",
    micrositeGitHostingUrl := "https://github.com/scalaz/scalaz",
    micrositeGithubOwner := "scalaz",
    micrositeGithubRepo := "scalaz",
    micrositeFavicons := Seq(microsites.MicrositeFavicon("favicon.png", "512x512")),
    micrositePalette := Map(
      "brand-primary"   -> "#ED2124",
      "brand-secondary" -> "#251605",
      "brand-tertiary"  -> "#491119",
      "gray-dark"       -> "#453E46",
      "gray"            -> "#837F84",
      "gray-light"      -> "#E3E2E3",
      "gray-lighter"    -> "#F4F3F4",
      "white-color"     -> "#FFFFFF"
    )
  )

lazy val laws = crossProject(JSPlatform, JVMPlatform)
  .in(file("laws"))
  .settings(stdSettings("laws"))
  .dependsOn(base)

lazy val lawsJVM = laws.jvm

lazy val lawsJS = laws.js

lazy val tests = project.module
  .dependsOn(baseJVM, lawsJVM)
  .settings(
    libraryDependencies += "org.scalaz" %% "testz-stdlib" % "0.0.3",
    libraryDependencies += "org.scalaz" %% "testz-runner" % "0.0.3"
  )
