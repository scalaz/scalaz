sealed trait ScalaVersion {
  def versionString: String = this match {
    case ReleaseVersion(a, b, c)       => List(a, b, c).mkString(".")
    case PreReleaseVersion(rv, suffix) => List(rv.versionString, suffix.suffixString).mkString("-")
  }
  def isModularised: Boolean = this match {
    case PreReleaseVersion(ReleaseVersion(2, n, minor), suffix) if n >= 11 => true
    case _ => false
  }
}
final case class ReleaseVersion(epoch: Int, major: Int, minor: Int) extends ScalaVersion
final case class PreReleaseVersion(approaching: ReleaseVersion,
                                   suffix: PreReleaseSuffix) extends ScalaVersion
sealed trait PreReleaseSuffix {
  def suffixString: String = this match {
    case M(n)     => s"M$n"
    case RC(n)    => s"RC$n"
    case SNAPSHOT => "SNAPSHOT"
  }
}
case object SNAPSHOT extends PreReleaseSuffix
case class M(n: Int) extends PreReleaseSuffix
case class RC(n: Int) extends PreReleaseSuffix

object ScalaVersion {
  def apply(version: String) =
    parseScalaVersion0(version).getOrElse(sys.error("could not parse: " + version))

  def parseScalaVersion0(version: String): Option[ScalaVersion] = {
    val R = """\.|-|(?<=M|RC)"""
    def parseSuffix(parts: List[String]): PreReleaseSuffix = parts match {
      case List("SNAPSHOT") => SNAPSHOT
      case List("M", n)     => M(n.toInt)
      case List("RC", n)    => RC(n.toInt)
      case _                => sys.error("could not parse: " + parts)
    }
    version.split(R).toList match {
      case List(epoch, major, minor) =>
        Some(ReleaseVersion(epoch.toInt, major.toInt, minor.toInt))
      case epoch :: major :: minor :: rest  =>
        val release = ReleaseVersion(epoch.toInt, major.toInt, minor.toInt)
        Some(PreReleaseVersion(release, parseSuffix(rest)))
      case _ =>
        None
    }
  }

  def testParseScalaVersion: Unit = {
    val testData = List(
      "2.10.0"          -> ReleaseVersion(2, 10, 0),
      "2.10.3-RC1"      -> PreReleaseVersion(ReleaseVersion(2, 10, 3), RC(1)),
      "2.11.0-M1"       -> PreReleaseVersion(ReleaseVersion(2, 11, 0), M(1)),
      "2.11.0-SNAPSHOT" -> PreReleaseVersion(ReleaseVersion(2, 11, 0), SNAPSHOT)
    )

    testData.foreach {
      case (in, expected) =>
        //println(in, ScalaVersion(in), expected)
        assert(ScalaVersion(in) == expected)
        assert(expected.versionString == in)
    }
  }
  testParseScalaVersion
}
