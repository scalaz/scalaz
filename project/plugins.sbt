scalacOptions += "-deprecation"

addSbtPlugin("com.eed3si9n" % "sbt-salad-days" % "0.2.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.6.1")
addSbtPlugin("com.github.sbt" % "sbt-release" % "1.5.0")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")

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
