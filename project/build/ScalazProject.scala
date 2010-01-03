import sbt._
import sbt.CompileOrder._
import java.util.jar.Attributes.Name._
import java.io.File
import scala.Array

abstract class ScalazDefaults(info: ProjectInfo) extends DefaultProject(info)
        with AutoCompilerPlugins{
  val scalaTools2_8_0Snapshots = Resolver.url("2.8.0 snapshots") artifacts "http://scala-tools.org/repo-snapshots/org/scala-lang/[module]/2.8.0-SNAPSHOT/[artifact]-[revision].[ext]"

  // This lets you use a local copy of scala. Set build.scala.versions=2.8.0-latest in build.properties.
  override def localScala = defineScala("2.8.0-latest", Path.userHome / "usr" / "scala-2.8.0.latest" asFile) :: Nil

  private val encodingUtf8 = List("-encoding", "UTF-8")

  override def compileOptions =
          encodingUtf8.map(CompileOption(_)) :::
          target(Target.Java1_5) :: Unchecked :: super.compileOptions.toList

  override def packageOptions = ManifestAttributes((IMPLEMENTATION_TITLE, "Scalaz"), (IMPLEMENTATION_URL, "http://code.google.com/p/scalaz"), (IMPLEMENTATION_VENDOR, "The Scalaz Project"), (SEALED, "true")) :: Nil

  override def documentOptions = encodingUtf8.map(SimpleDocOption(_))
  
  override def managedStyle = ManagedStyle.Maven

  override def packageDocsJar = defaultJarPath("-javadoc.jar")

  override def packageSrcJar = defaultJarPath("-sources.jar")

  override def packageTestSrcJar = defaultJarPath("-test-sources.jar")

  lazy val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)

  lazy val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadoc"), Nil, None)

  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc, packageTestSrc)
}

final class ScalazProject(info: ProjectInfo) extends ParentProject(info) with FileTasks {
  // Sub-projects
  lazy val core = project("core", "scalaz-core", new Core(_))
  lazy val http = project("http", "scalaz-http", new Http(_), core)
  lazy val example = project("example", "scalaz-example", new Example(_), core, http)
  lazy val scalacheckBinding = project("scalacheck-binding", "scalaz-scalacheck-binding", new ScalacheckBinding(_), core)
  lazy val tests = project("tests", "scalaz-test-suite", new TestSuite(_), core, scalacheckBinding)

  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)

  private def noAction = task {None}

  override def deliverLocalAction = noAction

  override def publishLocalAction = noAction

  override def publishAction = task {None}

  lazy val releaseZipAction = {
    val allJars = Path.lazyPathFinder(Seq(core, example, http).map(_.outputPath)).## ** GlobFilter("*jar")
    val extra = path("README")
    zipTask(allJars +++ extra, outputPath / "scalaz-full.zip")
  }

  def zipTask(sources: PathFinder, zipPath: => Path): Task =
    fileTask("zip", zipPath from sources) {FileUtilities.zip(sources.get, zipPath, false, log)}

  override def packageToPublishActions = super.packageToPublishActions ++ Seq(releaseZipAction)

  class Core(info: ProjectInfo) extends ScalazDefaults(info)

  class Http(info: ProjectInfo) extends ScalazDefaults(info) {
    val servlet = "javax.servlet" % "servlet-api" % "2.5" withSources
  }

  class ScalacheckBinding(info: ProjectInfo) extends ScalazDefaults(info) {
    val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.7-SNAPSHOT" withSources
  }

  class Example(info: ProjectInfo) extends ScalazDefaults(info) {
    val specs = "org.scala-tools.testing" % "specs" % "1.6.1-2.8.0.Beta1-RC5" % "test" withSources
  }

  class TestSuite(info: ProjectInfo) extends ScalazDefaults(info) {
    val specs = "org.scala-tools.testing" % "specs" % "1.6.1-2.8.0.Beta1-RC5" % "test" withSources
  }
}
