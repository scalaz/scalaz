scalacOptions += "-deprecation"

val scalaJsVersion = sys.env.get("SCALA_JS_VERSION").filter(_.trim.nonEmpty).getOrElse("0.6.27")

addSbtPlugin("com.47deg" % "sbt-microsites" % "0.7.22")
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "3.3.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.11")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.2")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "2.0.1")
addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.4")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.6.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJsVersion)
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.8")

// https://github.com/sbt/sbt/issues/2217
fullResolvers ~= {_.filterNot(_.name == "jcenter")}
