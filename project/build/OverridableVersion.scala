import sbt._

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
