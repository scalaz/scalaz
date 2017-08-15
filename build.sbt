
lazy val root = Project(
  id = "root",
  base = file(".")
).aggregate ( bazeJVM
            , bazeJS
            , metaJVM
            , metaJS
            , effectJVM
            , effectJS
            , benchmarks
).enablePlugins(ScalaJSPlugin)

lazy val baze         = crossProject.in(file("base"))
  .settings(stdSettings("base"))
  .dependsOn( meta )

lazy val bazeJVM      = baze.jvm

lazy val bazeJS       = baze.js

lazy val effect       = crossProject.in(file("effect"))
  .settings(stdSettings("effect"))
  .dependsOn( baze )

lazy val effectJVM    = effect.jvm

lazy val effectJS     = effect.js

lazy val benchmarks   = module("benchmarks")
  .dependsOn( bazeJVM, effectJVM )
  .enablePlugins(JmhPlugin)
  .settings(
    libraryDependencies ++=
      Seq ( "org.scala-lang"  %  "scala-reflect"  % scalaVersion.value
          , "org.scala-lang"  %  "scala-compiler" % scalaVersion.value % "provided"
          , "org.scalaz"      %% "scalaz-core"    % "7.2.7"
          , "io.monix"        %% "monix"          % "2.3.0"
          , "org.typelevel"   %% "cats-effect"    % "0.3")
  )

lazy val meta         = crossProject.in(file("meta"))
  .settings(stdSettings("meta"))
  .settings(
    libraryDependencies ++=
      Seq ( "org.scala-lang" % "scala-reflect" % scalaVersion.value
          , "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
          )
  )

lazy val metaJVM      = meta.jvm

lazy val metaJS       = meta.js
