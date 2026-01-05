scalacOptions += "-deprecation"

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.6.1")
addSbtPlugin("com.github.sbt" % "sbt-release" % "1.4.0")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "3.0.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.20.1")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.9")

// https://github.com/sbt/sbt/issues/2217
fullResolvers ~= {_.filterNot(_.name == "jcenter")}

if (sys.env.isDefinedAt("GITHUB_ACTION")) {
  Def.settings(
    addSbtPlugin("net.virtual-void" % "sbt-hackers-digest" % "0.1.2")
  )
} else {
  Nil
}

semanticdbEnabled := true

Global / onLoad := { state1 =>
  val state2 = {
    if (sys.props.isDefinedAt("check_build_file_imports")) {
      Project.extract(state1).runInputTask(Compile / scalafix, " OrganizeImports --check", state1)._1
    } else {
      state1
    }
  }
  (Global / onLoad).value.apply(state2)
}
