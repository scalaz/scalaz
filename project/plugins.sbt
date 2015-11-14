scalacOptions += "-deprecation"

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-osgi" % "0.7.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.3")

// https://github.com/sbt/sbt/issues/2217
fullResolvers ~= {_.filterNot(_.name == "jcenter")}
