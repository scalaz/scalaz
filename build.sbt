import Scalaz._

lazy val root = project
  .in(file("."))
  .aggregate(baseJVM, baseJS, metaJVM, metaJS, effectJVM, effectJS, example, benchmarks)
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
      Seq("org.specs2" %%% "specs2-core"          % "4.0.0" % "test",
          "org.specs2" %%% "specs2-matcher-extra" % "4.0.0" % "test"),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )
  .dependsOn(base)

lazy val effectJVM = effect.jvm

lazy val effectJS = effect.js

lazy val benchmarks = project.module
  .dependsOn(baseJVM, effectJVM)
  .enablePlugins(JmhPlugin)
  .settings(
    libraryDependencies ++=
      Seq(
        "org.scala-lang" % "scala-reflect"  % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
        "org.scalaz"     %% "scalaz-core"   % "7.2.7",
        "io.monix"       %% "monix"         % "3.0.0-M3",
        "org.typelevel"  %% "cats-effect"   % "0.10"
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

lazy val example = project.module
  .dependsOn(baseJVM)
  .enablePlugins(MicrositesPlugin)
  .settings(
    libraryDependencies += "com.github.ghik" %% "silencer-lib" % "0.5",
    micrositeFooterText := Some("""
                                  |<p>&copy; 2017 <a href="https://github.com/scalaz/scalaz">Scalaz Maintainers</a></p>
                                  |""".stripMargin),
    micrositeName := "Scalaz",
    micrositeDescription := "Scalaz examples",
    micrositeAuthor := "Scalaz contributors",
    micrositeOrganizationHomepage := "https://github.com/scalaz/scalaz",
    micrositeGitterChannelUrl := "scalaz/scalaz",
    micrositeGitHostingUrl := "https://github.com/scalaz/scalaz",
    micrositeGithubOwner := "scalaz",
    micrositeGithubRepo := "scalaz",
    micrositeFavicons := Seq(microsites.MicrositeFavicon("favicon.png", "512x512"))
  )
