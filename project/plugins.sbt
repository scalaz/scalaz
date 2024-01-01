scalacOptions += "-deprecation"

val scalaJsVersion = sys.env.get("SCALA_JS_VERSION").filter(_.trim.nonEmpty).getOrElse("1.15.0")
val scalaNativeVersion = sys.env.get("SCALA_NATIVE_VERSION").filter(_.trim.nonEmpty).getOrElse("0.4.16")

addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.0.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")
addSbtPlugin("com.github.sbt" % "sbt-release" % "1.1.0")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "3.0.2")
addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.6")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJsVersion)
addSbtPlugin("org.scala-native" % "sbt-scala-native" % scalaNativeVersion)
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.3")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.21")

// https://github.com/sbt/sbt/issues/2217
fullResolvers ~= {_.filterNot(_.name == "jcenter")}

if (sys.env.isDefinedAt("GITHUB_ACTION")) {
  Def.settings(
    addSbtPlugin("net.virtual-void" % "sbt-hackers-digest" % "0.1.2")
  )
} else {
  Nil
}
