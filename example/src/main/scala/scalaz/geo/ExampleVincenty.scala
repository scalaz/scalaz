package scalaz.geo

object ExampleVincenty {
  def main(args: Array[String]) = run

  import scalaz.Scalaz._

  def run {
    implicit val ellipsoid = wgs84

    val mountBarney = -28.2807 |-| 152.698
    val ngaPeak = -22.6528 |-| 167.4619

    val d = mountBarney.direct(bearing(11.5D), 150435D)

    // ((-26.950066610300084,153.00001065664318),Bearing {bearing = 11.359998078380356})
    val e = vector(0.005187494614261823D |-| 152.67721319546956D, bearing(-1.0663583638067147D))
    
    d.println // todo

    println(d ≟ e)
  }
}
