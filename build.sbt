import Scalaz._

cancelable in Global := true

organization in ThisBuild := "org.scalaz"

findLicense

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

lazy val root = project
  .in(file("."))
  .settings(
    skip in publish := true
  )
  .aggregate(baseJVM, baseJS, lawsJVM, lawsJS, tests, metaJVM, metaJS, microsite, benchmarks)
  .enablePlugins(ScalaJSPlugin)

resolvers in ThisBuild += "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"

lazy val base = crossProject.module
  .dependsOn(meta)
  .settings(libraryDependencies += "org.scalaz" %%% "scalaz-zio" % "0.2.9")

lazy val baseJVM = base.jvm

lazy val baseJS = base.js

lazy val benchmarks = project.module
  .dependsOn(baseJVM, tests)
  .enablePlugins(JmhPlugin)
  .settings(
    skip in publish := true,
    libraryDependencies ++=
      Seq(
        "org.scala-lang" % "scala-reflect"  % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
        "org.scalaz"     %% "scalaz-core"   % "7.2.26",
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

lazy val microsite = project.module
  .dependsOn(baseJVM, lawsJVM, tests)
  .enablePlugins(MicrositesPlugin, BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, isSnapshot),
    buildInfoPackage := "scalaz",
    buildInfoObject := "BuildInfo"
  )
  .settings(
    scalacOptions -= "-Yno-imports",
    scalacOptions ~= { _ filterNot (_ startsWith "-Ywarn") },
    scalacOptions ~= { _ filterNot (_ startsWith "-Xlint") },
    skip in publish := true,
    // Don't update silencer, the new versions print stuff that ends up in the generated code blocks
    libraryDependencies -= Scalaz.silencerPlugin,
    libraryDependencies += compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.0"),
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
    micrositeBaseUrl := "/8",
    micrositeDocumentationUrl := s"https://javadoc.io/doc/org.scalaz/scalaz-base_2.12/${(version in Compile).value}",
    micrositeDocumentationLabelDescription := "Scaladoc",
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

lazy val laws = crossProject.module
  .dependsOn(base)

lazy val lawsJVM = laws.jvm

lazy val lawsJS = laws.js

lazy val tests = project.module
  .dependsOn(baseJVM, lawsJVM)
  .settings(
    libraryDependencies += "org.scalaz" %% "testz-stdlib" % "0.0.5",
    libraryDependencies += "org.scalaz" %% "testz-runner" % "0.0.5",
    libraryDependencies += "org.scalaz" %% "testz-extras" % "0.0.5",
  )
