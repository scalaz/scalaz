package scalazdemo

import scalaz.Show._

/*
showv(empty: Stream[Int])
<>

showv(empty: Stream[Int])
<1>

showv(empty: Stream[Int])
<1,2,3>

showv((1 to 10).toStream)
<1,2,3,4,5,6,7,8,9,10>

showv(Nil: List[Int])
[]

showv(List(1))
[1]

showv(List(1, 2, 3))
[1,2,3]

showv((1 to 10).toList)
[1,2,3,4,5,6,7,8,9,10]

showv(new Array[Int](0))
{}

showv(Array(1))
{1}

showv(Array(1, 2, 3))
{1,2,3}

showv((1 to 10).toArray)
{1,2,3,4,5,6,7,8,9,10}
*/
object Show {
  import Stream.{cons, empty}

  val demoes = List(
    // ShowStream
    ("showv(empty: Stream[Int])", showv(empty: Stream[Int])),
    ("showv(empty: Stream[Int])", showv(cons(1, empty))),
    ("showv(empty: Stream[Int])", showv(cons(1, cons(2, cons(3, empty))))),
    ("showv((1 to 10).toStream)", showv((1 to 10).toStream)),

    // ShowList
    ("showv(Nil: List[Int])", showv(Nil: List[Int])),
    ("showv(List(1))", showv(List(1))),
    ("showv(List(1, 2, 3))", showv(List(1, 2, 3))),
    ("showv((1 to 10).toList)", showv((1 to 10).toList)),

    // ShowArray
    ("showv(new Array[Int](0))", showv(new Array[Int](0))),
    ("showv(Array(1))", showv(Array(1))),
    ("showv(Array(1, 2, 3))", showv(Array(1, 2, 3))),
    ("showv((1 to 10).toArray)", showv((1 to 10).toArray))
  )

  def main(args: Array[String]) {
    import Predef.println
    demoes.foreach { case (s, x) => {
      println(s)
      println(x.mkString)
      println
    } }
  }
}
