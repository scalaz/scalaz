package scalazdemo.control

/*
List(1, 2, 3, 4).suml
10

List("abc", "def", "ghi").suml
abcdefghi

List(1, 2, 3, 4).items
4

Some(4).items
1

None.items
0

List(1, 2, 3).rev[List]
List(3, 2, 1)

List(1, 2, 3).rev[Array]
Array(3, 2, 1)

List(1, 2, 3).rev[Option]
Some(3)

List(6, 9, 3, 7, 2).max
9

Some(7).max
7

List(6, 9, 3, 7, 2).min
2

Some(7).min
7
*/
object FoldLeft {
  import scalaz.control.FoldLeftW._
  import scalaz.control.Monoid.IntAdditionMonoid

  val demoes = List(
    // suml
    ("List(1, 2, 3, 4).suml", List(1, 2, 3, 4).suml),
    ("List(\"abc\", \"def\", \"ghi\").suml", List("abc", "def", "ghi").suml),

    // items
    ("List(1, 2, 3, 4).items", List(1, 2, 3, 4).items),
    ("Some(4).items", Some(4).items),
    ("None.items", None.items),

    // reverse
    ("List(1, 2, 3).rev[List]", List(1, 2, 3).rev[List]),
    ("List(1, 2, 3).rev[Array]", List(1, 2, 3).rev[Array]),
    ("List(1, 2, 3).rev[Option]", List(1, 2, 3).rev[Option]),

    // max
    ("List(6, 9, 3, 7, 2).max", List(6, 9, 3, 7, 2).max),
    ("Some(7).max", Some(7).max),

    // min
    ("List(6, 9, 3, 7, 2).min", List(6, 9, 3, 7, 2).min),
    ("Some(7).min", Some(7).min)
  )
  
  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
