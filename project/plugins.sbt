scalacOptions += "-deprecation"

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.4.0")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.4.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.7")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.23")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.8")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.7.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.1")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.2.0")

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
