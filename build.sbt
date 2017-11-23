import Scalaz._

lazy val root = project.in(file("."))
  .aggregate (baseJVM
            , baseJS
            , metaJVM
            , metaJS
            , effectJVM
            , effectJS
            , example
            , benchmarks
).enablePlugins(ScalaJSPlugin)

lazy val base         = crossProject.module
  .dependsOn( meta )

lazy val baseJVM      = base.jvm

lazy val baseJS       = base.js

lazy val effect       = crossProject.module
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
          , "io.monix"        %% "monix"          % "2.3.0"
          , "org.typelevel"   %% "cats-effect"    % "0.3")
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

lazy val example      = project.module
  .dependsOn( baseJVM )
  .enablePlugins(TutPlugin)
  .settings(libraryDependencies += "com.github.ghik" %% "silencer-lib" % "0.5")
