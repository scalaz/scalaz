import sbt._
import java.util.jar.Attributes.Name._

abstract class ScalazDefaults(info: ProjectInfo) extends DefaultProject(info) with OverridableVersion
with AutoCompilerPlugins {
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

  private val encodingUtf8 = List("-encoding", "UTF-8")

  override def compileOptions =
    encodingUtf8.map(CompileOption(_)) :::
        target(Target.Java1_5) :: Unchecked :: super.compileOptions.toList

  override def packageOptions = ManifestAttributes((IMPLEMENTATION_TITLE, "Scalaz"), (IMPLEMENTATION_URL, "http://code.google.com/p/scalaz"), (IMPLEMENTATION_VENDOR, "The Scalaz Project"), (SEALED, "true")) :: Nil

  override def documentOptions = encodingUtf8.map(SimpleDocOption(_)): List[ScaladocOption]

  override def managedStyle = ManagedStyle.Maven

  override def packageSrcJar = defaultJarPath("-sources.jar")

  override def packageTestSrcJar = defaultJarPath("-test-sources.jar")

  override def outputPattern = "[conf]/[type]/[artifact](-[revision])(-[classifier]).[ext]"

  lazy val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)

  def specsDependency = "org.scala-tools.testing" % "specs_2.9.0" % "1.6.8" % "test" withSources

  def scalacheckDependency = "org.scala-tools.testing" % "scalacheck_2.9.0.RC3" % "1.8"

  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc, packageTestSrc)

  // Workaround for problem described here: http://groups.google.com/group/simple-build-tool/browse_thread/thread/7575ea3c074ee8aa/373a91c25393085c?#373a91c25393085c
  override def deliverScalaDependencies = Nil

  //    override def consoleInit =
  //"""
  //import scalaz._
  //import Scalaz._
  //
  //"""
}

final class ScalazProject(info: ProjectInfo) extends ParentProject(info) with OverridableVersion {
  // Sub-projects
  lazy val core = project("core", "scalaz-core", new Core(_))
  lazy val geo = project("geo", "scalaz-geo", new Geo(_), core)
  lazy val example = project("example", "scalaz-example", new Example(_), core, geo)
  lazy val scalacheckBinding = project("scalacheck-binding", "scalaz-scalacheck-binding", new ScalacheckBinding(_), core)
  lazy val scalacheckGeo = project("geo-scalacheck", "scalaz-geo-scalacheck", new GeoScalacheck(_), core, scalacheckBinding, geo)
  lazy val tests = project("tests", "scalaz-test-suite", new TestSuite(_), core, geo, scalacheckBinding, scalacheckGeo)
  lazy val full = project("full", "scalaz-full", new Full(_), core, scalacheckBinding, example, tests)
  lazy val allModules = Seq(core, geo, example, scalacheckBinding, scalacheckGeo, tests)

  val pubishToRepoName = "Sonatype Nexus Repository Manager"

  val publishTo = {
    val repoUrl = "http://nexus.scala-tools.org/content/repositories/" + (if (version.toString.endsWith("-SNAPSHOT"))
      "snapshots"
    else
      "releases")

    pubishToRepoName at repoUrl
  }

  lazy val publishUser = system[String]("build.publish.user")
  lazy val publishPassword = system[String]("build.publish.password")

  (publishUser.get, publishPassword.get) match {
    case (Some(u), Some(p)) =>
      Credentials.add(pubishToRepoName, "nexus.scala-tools.org", u, p)
    case _ =>
      Credentials(Path.userHome / ".ivy2" / ".credentials", log)
  }

  // This lets you use a local copy of scala. Set build.scala.versions=2.8.0-custom in build.properties.
  override def localScala = defineScala("2.8.0-custom", Path.userHome / "usr" / "scala-2.8.0.r21276-b20100326020422" asFile) :: Nil

  private def noAction = task {
    None
  }

  override def deliverLocalAction = noAction

  override def publishLocalAction = noAction

  override def publishAction = task {
    None
  }

  val parentPath = path _

  class Core(info: ProjectInfo) extends ScalazDefaults(info) with Boilerplate {
    override def mainSourceRoots = super.mainSourceRoots +++ srcManagedScala ##

    override def compileAction = super.compileAction dependsOn (generateTupleW)

    override def documentOptions = documentTitle("Scalaz Core") :: super.documentOptions
  }

  class Geo(info: ProjectInfo) extends ScalazDefaults(info) {
    override def documentOptions = documentTitle("Scalaz Geodetic") :: super.documentOptions
  }

  class ScalacheckBinding(info: ProjectInfo) extends ScalazDefaults(info) {
    val scalacheck = scalacheckDependency

    override def documentOptions = documentTitle("Scalaz Scalacheck") :: super.documentOptions

    override def consoleInit = super.consoleInit +
        """
        import org.scalacheck._
        import org.scalacheck.Prop._
        """

  }

  class GeoScalacheck(info: ProjectInfo) extends ScalacheckBinding(info) {
    override val scalacheck = scalacheckDependency

    override def documentOptions = documentTitle("Scalaz Geo Scalacheck") :: super.documentOptions.tail
  }

  class Example(info: ProjectInfo) extends ScalazDefaults(info) {
    val specs = specsDependency

    override def documentOptions = documentTitle("Scalaz Example") :: super.documentOptions
  }

  class TestSuite(info: ProjectInfo) extends ScalazDefaults(info) {
    val specs = specsDependency

    override def documentOptions = documentTitle("Scalaz Tests") :: super.documentOptions
  }

  class Full(info: ProjectInfo) extends ScalazDefaults(info) {
    lazy val packageFullAction = packageFull

    lazy val packageFull = {
      val allJars = Path.lazyPathFinder(Seq(core, geo /*example,*/).map(_.outputPath)).## ** "*jar"
      val p = parentPath
      val extra = p("README") +++ p("etc").## ** "*"
      val sourceFiles = allJars +++ extra +++ (((outputPath ##) / "doc") ** "*")
      val packageName = "scalaz-full_" + buildScalaVersion + "-" + version.toString
      val copy = task {
        sbt.FileUtilities.copy(sourceFiles.get, outputPath / packageName, log)
        None
      }
      zipTask((outputPath ##) / packageName ** "*", outputPath / (packageName + ".zip")) dependsOn (copy)
    } describedAs ("Zip all artifacts")

    private def noAction = task {
      None
    }

    override def publishLocalAction = noAction dependsOn packageFullAction

    override def publishAction = noAction dependsOn packageFullAction

    def deepSources = Path.finder {
      topologicalSort.flatMap {
        case p: ScalaPaths => p.mainSources.getFiles
      }
    }

    def allSourceRoots = topologicalSort.flatMap {
      case p: ScalaPaths => p.mainSourceRoots.getFiles.map(_.getAbsolutePath)
    }

    val sxr = "lib" / "sxr_2.8.0.RC2-0.2.4-SNAPSHOT.jar"

    override def documentOptions =
      SimpleDocOption("-Xplugin:" + sxr.asFile.getAbsolutePath) ::
          SimpleDocOption("-P:sxr:base-directory:" + allSourceRoots.mkString(":")) ::
          super.documentOptions

    lazy val fullDoc = scaladocTask("scalaz", deepSources, docPath, docClasspath, documentOptions)

  }

}
