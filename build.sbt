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
  .aggregate(baseJVM, baseJS, metaJVM, metaJS, effectJVM, effectJS, stdJVM, stdJS, example, benchmarks, `scalaz-test`)
  .enablePlugins(ScalaJSPlugin)

lazy val base = crossProject.module
  .dependsOn(meta)

lazy val baseJVM = base.jvm

lazy val baseJS = base.js

lazy val effect = crossProject
  .in(file("effect"))
  .settings(stdSettings("effect"))
  .settings(
    libraryDependencies ++=
      Seq("org.specs2" %%% "specs2-core"          % "4.2.0" % "test",
          "org.specs2" %%% "specs2-matcher-extra" % "4.2.0" % "test"),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )
  .dependsOn(base)

lazy val effectJVM = effect.jvm

lazy val effectJS = effect.js

lazy val benchmarks = project.module
  .dependsOn(baseJVM, effectJVM)
  .enablePlugins(JmhPlugin)
  .settings(
    skip in publish := true,
    libraryDependencies ++=
      Seq(
        "org.scala-lang" % "scala-reflect"  % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
        "org.scalaz"     %% "scalaz-core"   % "7.2.23",
        "io.monix"       %% "monix"         % "3.0.0-RC1",
        "org.typelevel"  %% "cats-effect"   % "1.0.0-RC"
      )
  )

lazy val meta = crossProject.module
  .settings(
    libraryDependencies ++=
      Seq("org.scala-lang" % "scala-reflect"  % scalaVersion.value,
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided")
  )

lazy val metaJVM = meta.jvm

lazy val metaJS = meta.js

lazy val std = crossProject.module
  .in(file("std"))
  .dependsOn(base)

lazy val stdJVM = std.jvm
lazy val stdJS  = std.js

lazy val example = project.module
  .dependsOn(baseJVM, stdJVM)
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
    micrositeDescription := "Scalaz examples",
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

lazy val `scalaz-test` = project
  .dependsOn(baseJVM, effectJVM)
  .in(file("scalaz-test"))
  .settings(stdSettings("scalaz-test"))

/** A project just for the console.
 * Applies only the settings necessary for that purpose.
 */
lazy val repl = project
  .dependsOn(`scalaz-test` % "compile->test")
  .dependsOn(benchmarks)
  .settings(
    console := (console in Test).value,
    scalacOptions --= Seq("-Yno-imports", "-Ywarn-unused:imports", "-Xfatal-warnings"),
    initialCommands in console += "import scalaz._, Scalaz._, scalaz.test._\n"
  )
