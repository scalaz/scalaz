scalacOptions += "-deprecation"

val scalaJsVersion = sys.env.get("SCALA_JS_VERSION").filter(_.trim.nonEmpty).getOrElse("1.3.1")

addSbtPlugin("com.47deg" % "sbt-microsites" % "1.2.1")
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "4.1.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.13")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.1.1")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "3.0.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.9.6")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJsVersion)
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.5")
addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.5.1")

// https://github.com/sbt/sbt/issues/2217
fullResolvers ~= {_.filterNot(_.name == "jcenter")}
