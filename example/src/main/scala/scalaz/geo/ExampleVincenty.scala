package scalaz.geo

object ExampleVincenty {
  def main(args: Array[String]) = run

  import scalaz.Scalaz._

  def run {
    val mountBarney = -28.2807 |-| 152.698
    val ngaPeak = -22.6528 |-| 167.4619
    implicit val ellipsoid = wgs84

    val d = mountBarney.direct(bearing(11.5D), 150435D)
    println(d) // todo
  }
}
