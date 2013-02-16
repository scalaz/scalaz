resolvers += Resolver.url("scalasbt", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.6")

addSbtPlugin("com.typesafe.sbtosgi" % "sbtosgi" % "0.3.0")
