scalacOptions += "-deprecation"

addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.1")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.2")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.20")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.6")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.7.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.1")

addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "1.1.0")

// https://github.com/sbt/sbt/issues/2217
fullResolvers ~= {_.filterNot(_.name == "jcenter")}
