scalacOptions += "-deprecation"

addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.2")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.3")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.20")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.6")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.7.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.1")

// https://github.com/ThoughtWorksInc/sbt-api-mappings/issues/8
libraryDependencies ++= {
  if (System.getProperty("java.specification.version") == "1.8")
    Defaults.sbtPluginExtra(
      m = "com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "1.1.0",
      sbtV = sbtBinaryVersion.value,
      scalaV = scalaBinaryVersion.value
    ) :: Nil
  else
    Nil
}

// https://github.com/sbt/sbt/issues/2217
fullResolvers ~= {_.filterNot(_.name == "jcenter")}
