package scalazdemo.control

/*
List(1, 2, 3, 4).sumr
10

List("abc", "def", "ghi").sumr
abcdefghi

shows(List(1, 2, 3, 4).stream)
<1,2,3,4>

shows(Some(7).stream)
<7>

List(1, 2, 3, 4) ! 0
1

List(1, 2, 3, 4) ! 2
3

Some(7) ! 0
7

List(1, 2, 3).intercalate[List](List(7, 8, 9))
List(1, 7, 8, 9, 2, 7, 8, 9, 3)

List(1, 2, 3).intercalate[Array](Array(7, 8, 9))
Array(1, 7, 8, 9, 2, 7, 8, 9, 3)

List(1, 2, 3) any even
true

List(1, 3, 5) any even
false

List(1, 2, 3) all even
false

List(2, 4, 6) all even
true

List(2, 4, 6) isEmpty
false

List() isEmpty
true

Some(7) isEmpty
false

None isEmpty
true

List(1, 2, 3, 4, 5, 6).select[List](even)
List(2, 4, 6)

List(1, 2, 3, 4, 5, 6).select[Array](even)
Array(2, 4, 6)

List(1, 2, 3, 4, 5, 6).select[Option](even)
Some(2)

List(3, 5, 7).select[Option](even)
None

Some(7).select[List](even)
List()

Some(8).select[List](even)
List(8)

none[Int].select[List](even)
List()

List(2, 4, 5, 6).selectWhile[List](even)
List(2, 4)

List(2, 4, 5, 6).selectWhile[Array](even)
Array(2, 4)

List(2, 4, 5, 6).selectWhile[Option](even)
Some(2)

List(3, 4, 7).selectWhile[Option](even)
None

Some(7).selectWhile[List](even)
List()

Some(8).selectWhile[List](even)
List(8)

none[Int].selectWhile[List](even)
List()

List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) splitWith even
List(List(2, 6, 8), List(9), List(6), List(7, 3, 5), List(8, 6), List(9))

Some(7) splitWith even
List(List(7))

Some(8) splitWith even
List(List(8))

none[Int] splitWith even
List()

List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) selectSplit even
List(List(2, 6, 8), List(6), List(8, 6))

Some(7) selectSplit even
List()

Some(8) selectSplit even
List(List(8))

none[Int] selectSplit even
List()

"age=54&name=Bob&address=At Home".toList selectSplit (_ != '&') map (_.mkString)
List(age=54, name=Bob, address=At Home)

List(1, 2, 4, 5).filterMonad[List, List](n => (1 to n).map(even).toList)
List(List(), List(5), List(), List(5), List(), List(4), List(4, 5), List(4), List(4, 5), List(4), List(), List(5), List(), List(5), List(), List(4), List(4, 5), List(4), List(4, 5), List(4), List(2), List(2, 5), List(2), List(2, 5), List(2), List(2, 4), List(2, 4, 5), List(2, 4), List(2, 4, 5), List(2, 4), List(2), List(2, 5), List(2), List(2, 5), List(2), List(2, 4), List(2, 4, 5), List(2, 4), List(2, 4, 5), List(2, 4))

Array(1, 4, 5).filterMonad[List, Array](n => (1 to n).map(even).toList)
List(Array(), Array(5), Array(), Array(5), Array(), Array(4), Array(4, 5), Array(4), Array(4, 5), Array(4), Array(), Array(5), Array(), Array(5), Array(), Array(4), Array(4, 5), Array(4), Array(4, 5), Array(4))

Array(1, 4, 5).filterMonad[List, Option](n => (1 to n).map(even).toList)
List(None, Some(5), None, Some(5), None, Some(4), Some(4), Some(4), Some(4), Some(4), None, Some(5), None, Some(5), None, Some(4), Some(4), Some(4), Some(4), Some(4))

List(1, 2).foldrMonad[Int, List](3, (a, b) => (1 to a + b).toList)
List(1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6)
*/
object FoldRight {
  import scalaz.control.FoldRightW._
  import scalaz.control.Monoid.IntAdditionMonoid
  import scalaz.OptionW._
  import scalaz.Show.shows

  val even = ((_: Int) % 2 == 0)

  val demoes = List(
    // sumr
    ("List(1, 2, 3, 4).sumr", List(1, 2, 3, 4).sumr),
    ("List(\"abc\", \"def\", \"ghi\").sumr", List("abc", "def", "ghi").sumr),

    // stream
    ("shows(List(1, 2, 3, 4).stream)", shows(List(1, 2, 3, 4).stream)),
    ("shows(Some(7).stream)", shows(Some(7).stream)),

    // !
    ("List(1, 2, 3, 4) ! 0", List(1, 2, 3, 4) ! 0),
    ("List(1, 2, 3, 4) ! 2", List(1, 2, 3, 4) ! 2),
    ("Some(7) ! 0", Some(7) ! 0),

    // intercalate
    ("List(1, 2, 3).intercalate[List](List(7, 8, 9))", List(1, 2, 3).intercalate[List](List(7, 8, 9))),
    ("List(1, 2, 3).intercalate[Array](Array(7, 8, 9))", List(1, 2, 3).intercalate[Array](Array(7, 8, 9))),

    // any
    ("List(1, 2, 3) any even", List(1, 2, 3) any even),
    ("List(1, 3, 5) any even", List(1, 3, 5) any even),

    // all
    ("List(1, 2, 3) all even", List(1, 2, 3) all even),
    ("List(2, 4, 6) all even", List(2, 4, 6) all even),

    // isEmpty
    ("List(2, 4, 6) isEmpty", List(2, 4, 6) isEmpty),
    ("List() isEmpty", List() isEmpty),
    ("Some(7) isEmpty", Some(7) isEmpty),
    ("None isEmpty", None isEmpty),

    // select
    ("List(1, 2, 3, 4, 5, 6).select[List](even)", List(1, 2, 3, 4, 5, 6).select[List](even)),
    ("List(1, 2, 3, 4, 5, 6).select[Array](even)", List(1, 2, 3, 4, 5, 6).select[Array](even)),
    ("List(1, 2, 3, 4, 5, 6).select[Option](even)", List(1, 2, 3, 4, 5, 6).select[Option](even)),
    ("List(3, 5, 7).select[Option](even)", List(3, 5, 7).select[Option](even)),
    ("Some(7).select[List](even)", Some(7).select[List](even)),
    ("Some(8).select[List](even)", Some(8).select[List](even)),
    ("none[Int].select[List](even)", none[Int].select[List](even)),

    // selectWhile
    ("List(2, 4, 5, 6).selectWhile[List](even)", List(2, 4, 5, 6).selectWhile[List](even)),
    ("List(2, 4, 5, 6).selectWhile[Array](even)", List(2, 4, 5, 6).selectWhile[Array](even)),
    ("List(2, 4, 5, 6).selectWhile[Option](even)", List(2, 4, 5, 6).selectWhile[Option](even)),
    ("List(3, 4, 7).selectWhile[Option](even)", List(3, 4, 7).selectWhile[Option](even)),
    ("Some(7).selectWhile[List](even)", Some(7).selectWhile[List](even)),
    ("Some(8).selectWhile[List](even)", Some(8).selectWhile[List](even)),
    ("none[Int].selectWhile[List](even)", none[Int].selectWhile[List](even)),

    // splitWith
    ("List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) splitWith even", List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) splitWith even),
    ("Some(7) splitWith even", Some(7) splitWith even),
    ("Some(8) splitWith even", Some(8) splitWith even),
    ("none[Int] splitWith even", none[Int] splitWith even),

    // selectSplit
    ("List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) selectSplit even", List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) selectSplit even),
    ("Some(7) selectSplit even", Some(7) selectSplit even),
    ("Some(8) selectSplit even", Some(8) selectSplit even),
    ("none[Int] selectSplit even", none[Int] selectSplit even),
    ("\"age=54&name=Bob&address=At Home\".toList selectSplit (_ != '&') map (_.mkString)", "age=54&name=Bob&address=At Home".toList selectSplit (_ != '&') map (_.mkString)),

    // filterMonad
    ("List(1, 2, 4, 5).filterMonad[List, List](n => (1 to n).map(even).toList)", List(1, 2, 4, 5).filterMonad[List, List](n => (1 to n).map(even).toList)),
    ("Array(1, 4, 5).filterMonad[List, Array](n => (1 to n).map(even).toList)", Array(1, 4, 5).filterMonad[List, Array](n => (1 to n).map(even).toList)),
    ("Array(1, 4, 5).filterMonad[List, Option](n => (1 to n).map(even).toList)", Array(1, 4, 5).filterMonad[List, Option](n => (1 to n).map(even).toList)),

    // foldrMonad
    ("List(1, 2).foldrMonad[List](3, (a, (b: Int)) => (1 to a + b).toList)", List(1, 2).foldrMonad[List](3, (a, (b: Int)) => (1 to a + b).toList))
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
