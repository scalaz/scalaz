import sbt._
import java.util.jar.Attributes.Name._

final class ScalazProject(info: ProjectInfo) extends DefaultProject(info) {
  val servlet = "javax.servlet" % "servlet-api" % "2.5"

  override def compileOrder = CompileOrder.JavaThenScala

  override def compileOptions = target(Target.Java1_5) :: Unchecked :: super.compileOptions.toList

  override def packageOptions = ManifestAttributes((IMPLEMENTATION_TITLE, "Scalaz"), (IMPLEMENTATION_URL, "http://code.google.com/p/scalaz"), (IMPLEMENTATION_VENDOR, "The Scalaz Project"), (SEALED, "true")) :: Nil

  override def documentOptions = documentTitle("Scalaz " + projectVersion + " API Specification") :: windowTitle("Scalaz " + projectVersion) ::
      super.documentOptions.toList
}
