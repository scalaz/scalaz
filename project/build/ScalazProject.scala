import sbt._
import sbt.CompileOrder._
import java.util.jar.Attributes.Name._
import java.io.File
import scala.Array

abstract class ScalazDefaults(info: ProjectInfo) extends DefaultProject(info) with OverridableVersion
        with AutoCompilerPlugins {
  // val scalaTools2_8_0Snapshots = Resolver.url("2.8.0 snapshots") artifacts "http://scala-tools.org/repo-snapshots/org/scala-lang/[module]/2.8.0-SNAPSHOT/[artifact]-[revision].[ext]"
  val scalaToolsSnapshots = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"

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

  def specsDependency = "org.scala-tools.testing" % "specs_2.8.0.Beta1" % "1.6.4-SNAPSHOT" % "test" withSources

  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc, packageTestSrc)
}

/**
 * Replaces 'SNAPSHOT' in the project version with the contents of the system property 'build.timestamp',
 * if provided. 
 */
trait OverridableVersion extends Project {
  lazy val buildTimestamp = system[String]("build.timestamp")
  
  override def version = {
    val realVersion = super.version
    val v = realVersion.toString
    val SnapshotVersion = """(.+)-SNAPSHOT""".r
    (buildTimestamp.get, realVersion.toString) match {
      case (Some(timestamp), SnapshotVersion(base)) => OpaqueVersion(base + "-" + timestamp)
      case _ => realVersion
    }
  }
}

final class ScalazProject(info: ProjectInfo) extends ParentProject(info) with OverridableVersion {
  // Sub-projects
  lazy val core = project("core", "scalaz-core", new Core(_))
  lazy val http = project("http", "scalaz-http", new Http(_), core)
  lazy val example = project("example", "scalaz-example", new Example(_), core, http)
  lazy val scalacheckBinding = project("scalacheck-binding", "scalaz-scalacheck-binding", new ScalacheckBinding(_), core)
  lazy val tests = project("tests", "scalaz-test-suite", new TestSuite(_), core, scalacheckBinding)
  lazy val full = project("full", "scalaz-full", new Full(_), core, scalacheckBinding, http, example, tests)
  lazy val allModules = Seq(core, http, example, scalacheckBinding, tests)

  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)

  // This lets you use a local copy of scala. Set build.scala.versions=2.8.0-custom in build.properties.  
  override def localScala = defineScala("2.8.0-custom", Path.userHome / "usr" / "scala-2.8.0.r21276-b20100326020422" asFile) :: Nil

  private def noAction = task {None}

  override def deliverLocalAction = noAction

  override def publishLocalAction = noAction

  override def publishAction = task {None}

  class Core(info: ProjectInfo) extends ScalazDefaults(info)

  class Http(info: ProjectInfo) extends ScalazDefaults(info) {
    val servlet = "javax.servlet" % "servlet-api" % "2.5" withSources
  }

  class ScalacheckBinding(info: ProjectInfo) extends ScalazDefaults(info) {
    val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.0.Beta1" % "1.7-SNAPSHOT" withSources
  }

  class Example(info: ProjectInfo) extends ScalazDefaults(info) {
    val specs = specsDependency
  }

  class TestSuite(info: ProjectInfo) extends ScalazDefaults(info) {
    val specs = specsDependency
  }

  class Full(info: ProjectInfo) extends ScalazDefaults(info) {
    def packageFullAction = packageFull dependsOn(fullDoc)
    
    def packageFull = {
      val allJars = Path.lazyPathFinder(Seq(core, example, http).map(_.outputPath)).## ** GlobFilter("*jar")
      val p = ScalazProject.this.path _
      val extra = p("README") +++ p("etc").## ** GlobFilter("*")
      val sourceFiles = allJars +++ extra +++ (((outputPath ##) / "doc") ** GlobFilter("*"))
      zipTask(sourceFiles, outputPath / ("scalaz-full_" + buildScalaVersion + "-" + version.toString + ".zip") )
    } describedAs("Zip all artifacts")
    
    private def noAction = task {None}
    
    override def publishLocalAction = noAction dependsOn packageFullAction

    override def publishAction = noAction dependsOn packageFullAction
    
    def deepSources = Path.finder { topologicalSort.flatMap { case p: ScalaPaths => p.mainSources.getFiles } }
  	lazy val fullDoc = scaladocTask("scalaz", deepSources, docPath, docClasspath, documentOptions)
  }
}
