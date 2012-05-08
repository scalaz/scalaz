resolvers += Resolver.url("scalasbt", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)


addSbtPlugin("com.typesafe.sbtosgi" % "sbtosgi" % "0.2.0-SNAPSHOT")
