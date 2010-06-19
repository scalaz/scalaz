import sbt._
import xsbt.FileUtilities._

trait ScalaVersionBumper extends Project {

  lazy val bumpScalaVersion: MethodTask =
  task {args: Seq[String] =>
    if (args.length == 1)
      bumpScalaVersion(args(0))
    else
      task {Some("Usage: bumpScalaVersion <string>")}
  } describedAs("Update build.properties and all IntelliJ project files with a new Scala version.")

  def bumpScalaVersion(newVersion: String) = task {
    val intellijFiles = path(".") ** ("*.iml" | "scala.xml")
    for (f <- intellijFiles.getFiles) {
      val oldVersion = buildScalaVersion
      val oldText = read(f)
      val newText = oldText.replaceAll("\\Q" + oldVersion, newVersion)
      log.info("Updating: " + f.getAbsolutePath)
      write(f, newText)
    }
    buildScalaVersions() = newVersion
    None
  }
}