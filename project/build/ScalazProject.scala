import sbt._
import sbt.CompileOrder._
import java.util.jar.Attributes.Name._
import java.io.File

abstract class ScalazDefaults(info: ProjectInfo, component: String) extends DefaultProject(info) {
  override def compileOptions = target(Target.Java1_5) :: Unchecked :: super.compileOptions.toList
  override def packageOptions = ManifestAttributes((IMPLEMENTATION_TITLE, "Scalaz"), (IMPLEMENTATION_URL, "http://code.google.com/p/scalaz"), (IMPLEMENTATION_VENDOR, "The Scalaz Project"), (SEALED, "true")) :: Nil
  override def documentOptions = documentTitle("Scalaz " + component + projectVersion + " API Specification") :: windowTitle("Scalaz " + projectVersion) :: super.documentOptions.toList
  //  override def defaultJarBaseName = "scalaz-" + component.toLowerCase + "-" + version.toString

  override def managedStyle = ManagedStyle.Maven

  override def packageDocsJar = defaultJarPath("-javadoc.jar")
  override def packageSrcJar = defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)
  val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadoc"), Nil, None)

  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)

  // override def fork = Some(new ForkScalaCompiler {
  //     override def javaHome: Option[File] = None
  //     override def scalaJars: Iterable[File] = List(
  //       new File("/Users/nkpart/p/x/am-scala/lib/scala-compiler.jar"),
  //       new File("/Users/nkpart/p/x/am-scala/lib/scala-library.jar")
  //       )
  //   }
  //   )
}

final class ScalazProject(info: ProjectInfo) extends ParentProject(info) {
  // Sub-projects
  lazy val core = project("core", "Scalaz Core", new ScalazCoreProject(_))
  lazy val test = project("test", "Scalaz Test", new ScalazTestProject(_), core)
  lazy val http = project("http", "Scalaz HTTP", new ScalazHttpProject(_), core)
  lazy val scapps = project("scapps", "Scalaz Scapps", new ScalazScappsProject(_), core, http)

  // Use this as a test run of pom generation and jar publishing
  // val publishTo = Resolver.file("local-file-repo", new java.io.File("/Users/jason/code/scalaz-maven/snapshots"))

  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)

  override def publishAction = task { None }

  // TODO Package the project up
  //packageProjectZip
  //  def extraResources = descendents(info.projectPath / "licenses", "*") +++ "LICENSE" +++ "NOTICE"
  //  override def mainResources = super.mainResources +++ extraResources

  // One-shot build for users building from trunk
  //  lazy val fullBuild = task {None} dependsOn (boot.proguard, main.crossPublishLocal) describedAs
  //      "Builds the loader and builds main sbt against all supported versions of Scala and installs to the local repository."
}

protected final class ScalazCoreProject(info: ProjectInfo) extends ScalazDefaults(info, "Core")

protected final class ScalazTestProject(info: ProjectInfo) extends ScalazDefaults(info, "Test") {
  val fjRepo = "Functional Java Repository" at "http://functionaljava.googlecode.com/svn/maven"
  val scalacheck = "org.scala-tools.testing" % "scalacheck" % "1.5" withSources
  val functionaljava = "org.functionaljava" % "fj" % "2.19" withSources
}

protected final class ScalazHttpProject(info: ProjectInfo) extends ScalazDefaults(info, "HTTP") {
  val servlet = "javax.servlet" % "servlet-api" % "2.5" withSources
}

protected final class ScalazScappsProject(info: ProjectInfo) extends ScalazDefaults(info, "Scapps") {
}
