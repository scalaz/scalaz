lazy val root = Project(
  id = "root",
  base = file(".")
).settings(
  scalaVersion := "2.11.8"
).aggregate ( baze
            , meta
            , benchmarks )

lazy val baze         = module("base")
  .dependsOn( meta )

lazy val benchmarks   = module("benchmarks")
  .dependsOn( baze )
  .enablePlugins(JmhPlugin)
  .settings(
    libraryDependencies ++=
      Seq ( "org.scala-lang" % "scala-reflect" % scalaVersion.value
          , "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
          , "org.scalaz" %% "scalaz-core" % "7.2.7")
  )

lazy val meta         = module("meta")
  .settings(
    libraryDependencies ++=
      Seq ( "org.scala-lang" % "scala-reflect" % scalaVersion.value
          , "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided" )
  )
