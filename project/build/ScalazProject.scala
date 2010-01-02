import sbt._
import sbt.CompileOrder._
import java.util.jar.Attributes.Name._
import java.io.File
import scala.Array

abstract class ScalazDefaults(info: ProjectInfo, component: String) extends DefaultProject(info)
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

  val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)

  val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadoc"), Nil, None)

  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)
}

final class ScalazProject(info: ProjectInfo) extends ParentProject(info) with FileTasks {
  // Sub-projects
  lazy val core = project("core", "Scalaz Core", new ScalazCoreProject(_))
  lazy val http = project("http", "Scalaz HTTP", new ScalazHttpProject(_), core)
  lazy val example = project("example", "Scalaz Example", new ScalazExampleProject(_), core, http)
  lazy val scalacheckBinding = project("scalacheck-binding", "Scalaz Scalacheck Binding", new ScalazScalacheckBindingProject(_), core)
  lazy val tests = project("tests", "Scalaz Test Suite", new ScalazTestsProject(_), core, scalacheckBinding)

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

  class ScalazCoreProject(info: ProjectInfo) extends ScalazDefaults(info, "Core")

  class ScalazExampleProject(info: ProjectInfo) extends ScalazDefaults(info, "Example") {
    val specs = "org.scala-tools.testing" % "specs" % "1.6.1-2.8.0.Beta1-RC5" % "test" withSources
  }

  class ScalazHttpProject(info: ProjectInfo) extends ScalazDefaults(info, "HTTP") {
    val servlet = "javax.servlet" % "servlet-api" % "2.5" withSources
  }

  class ScalazScalacheckBindingProject(info: ProjectInfo) extends ScalazDefaults(info, "Scalacheck Binding") {
    val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.7-SNAPSHOT" withSources
  }

  class ScalazTestsProject(info: ProjectInfo) extends ScalazDefaults(info, "Tests") {
    val specs = "org.scala-tools.testing" % "specs" % "1.6.1-2.8.0.Beta1-RC5" % "test" withSources
  }
}
