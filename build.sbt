import Scalaz._

lazy val root = project.in(file("."))
  .aggregate (baseJVM
            , baseJS
            , metaJVM
            , metaJS
            , effectJVM
            , effectJS
            , exampleJVM
            , exampleJS
            , benchmarks
).enablePlugins(ScalaJSPlugin)

lazy val base         = crossProject.module
  .dependsOn( meta )

lazy val baseJVM      = base.jvm

lazy val baseJS       = base.js

lazy val effect       = crossProject.in(file("effect"))
  .settings(stdSettings("effect"))
  .settings(
    libraryDependencies ++=
      Seq ( "org.specs2" %%% "specs2-core"           % "4.0.0" % "test"
          , "org.specs2" %%% "specs2-scalacheck"     % "4.0.0" % "test"
          , "org.specs2" %%% "specs2-matcher-extra"  % "4.0.0" % "test"),
      scalacOptions in Test ++= Seq("-Yrangepos"))
  .dependsOn( base )

lazy val effectJVM    = effect.jvm

lazy val effectJS     = effect.js

lazy val benchmarks   = project.module
  .dependsOn( baseJVM, effectJVM )
  .enablePlugins(JmhPlugin)
  .settings(
    libraryDependencies ++=
      Seq ( "org.scala-lang"  %  "scala-reflect"  % scalaVersion.value
          , "org.scala-lang"  %  "scala-compiler" % scalaVersion.value % "provided"
          , "org.scalaz"      %% "scalaz-core"    % "7.2.7"
          , "io.monix"        %% "monix"          % "3.0.0-M1"
          , "org.typelevel"   %% "cats-effect"    % "0.4")
  )

lazy val meta         = crossProject.module
  .settings(
    libraryDependencies ++=
      Seq ( "org.scala-lang" % "scala-reflect" % scalaVersion.value
          , "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
          )
  )

lazy val metaJVM      = meta.jvm

lazy val metaJS       = meta.js

lazy val example      = crossProject.module
  .dependsOn( base )

lazy val exampleJVM   = example.jvm

lazy val exampleJS    = example.js
