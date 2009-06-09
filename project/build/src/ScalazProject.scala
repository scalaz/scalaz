import sbt._
import java.util.jar.Attributes.Name._

final class ScalazProject(info: ProjectInfo) extends DefaultProject(info) {
  override def compileOptions = target(Target.Java1_5) :: Unchecked :: Optimize :: super.compileOptions.toList

  override def packageOptions = ManifestAttributes((IMPLEMENTATION_TITLE, "Scalaz"), (IMPLEMENTATION_URL, "http://code.google.com/p/scalaz"),
    (IMPLEMENTATION_VENDOR, "The Scalaz Project"), (SEALED, "true")) :: Nil

  override def documentOptions = documentTitle("Scalaz " + projectVersion + " API Specification") :: windowTitle("Scalaz " + projectVersion) ::
          super.documentOptions.toList

}
