package x


object T {
  import scalaz._, Scalaz._

  def main(args: Array[String]) {
    val f = 1 + (_: Int)
    val f2 = List(f, f).sumr
    println(f2(7))
  }
}