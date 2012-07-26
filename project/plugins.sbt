resolvers += Resolver.url("scalasbt", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.5")

// Add the following to z_local.sbt (which is .gitignored), to perform a release.
//
// addSbtPlugin("com.jsuereth" % "xsbt-gpg-plugin" % "0.6")
//
// addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.5")
