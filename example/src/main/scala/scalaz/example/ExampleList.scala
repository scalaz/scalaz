package scalaz.example


object ExampleList {
  def main(args: Array[String]) = run

  import scalaz._
  import Scalaz._

  def run {
    List(1, 2, 3) intersperse 0 assert_≟ List(1, 0, 2, 0, 3)
    List(1, 2) intersperse 0 assert_≟ List(1, 0, 2)
    List(1) intersperse 0 assert_≟ List(1)
    nil[Int] intersperse 0 assert_≟ nil[Int]

    List(1, 2, 3) intercalate List(-1, -2) assert_≟ List(1, -1, -2, 2, -1, -2, 3)
    List(1, 2, 3) intercalate nil[Int] assert_≟ List(1, 2, 3)
    List(1) intercalate nil[Int] assert_≟ List(1)
    nil[Int] intercalate nil[Int] assert_≟ nil[Int]
  }
}
