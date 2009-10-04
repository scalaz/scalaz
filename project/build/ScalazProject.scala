import sbt._
import java.util.jar.Attributes.Name._

final class ScalazProject(info: ProjectInfo) extends DefaultProject(info) {
  val servlet = "javax.servlet" % "servlet-api" % "2.5"

  override def compileOrder = CompileOrder.JavaThenScala

  override def compileOptions = target(Target.Java1_5) :: Unchecked :: super.compileOptions.toList

  override def packageOptions = ManifestAttributes((IMPLEMENTATION_TITLE, "Scalaz"), (IMPLEMENTATION_URL, "http://code.google.com/p/scalaz"), (IMPLEMENTATION_VENDOR, "The Scalaz Project"), (SEALED, "true")) :: Nil

  override def documentOptions = documentTitle("Scalaz " + projectVersion + " API Specification") :: windowTitle("Scalaz " + projectVersion) ::
      super.documentOptions.toList

  // todo configure direct publishing once credentials for scala-tools are obtained.
  override def managedStyle = ManagedStyle.Maven
  val localFileRepo = Resolver.file("local-file-repo", new java.io.File("/Users/jason/code/scalaz-maven/snapshots")) 
  val publishTo = localFileRepo

  override def packageDocsJar = defaultJarPath("-javadoc.jar")
  override def packageSrcJar= defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)
  val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadoc"), Nil, None)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc)
}
