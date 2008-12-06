package scalazdemo

import scalaz.EitherW._
import scalaz.EitherW.LeftProjectionW._
import scalaz.EitherW.RightProjectionW._

/*
~Left(7)
Right(7)

~Right(7)
Left(7)

Left(7) ? ("foo", "bar")
foo

Right(7) ? ("foo", "bar")
bar

left[Int](7).left value (_ + 1)
7

left[Int](7).right value (_ + 1)
8

right[Int, Int](7).left value (_ + 1)
8

right[Int, Int](7).right value (_ + 1)
7

left[Int](7).left(Left((_: Int) + 1))
Left(8)

left[Int](7).left(Right((_: Int) + 1))
Right(<function>)

left[Int](7).right(Left((_: Int) + 1))
Left(<function>)

left[Int](7).right(Right((_: Int) + 1))
Left(7)

right[Int](7).left(Left((_: Int) + 1))
Right(7)

right[Int](7).left(Right((_: Int) + 1))
Right(<function>)

right[Int](7).right(Left((_: Int) + 1))
Left(<function>)

right[Int](7).right(Right((_: Int) + 1))
Right(8)

joinLeft(Left(Left(7)))
Left(7)

joinLeft(Left(Right(7)))
Right(7)

joinLeft(Right(Left(7)))
Right(Left(7))

joinLeft(Right(Right(7)))
Right(Right(7))

joinRight(Left(Left(7)))
Left(Left(7))

joinRight(Left(Right(7)))
Left(Right(7))

joinRight(Right(Left(7)))
Left(7)

joinRight(Right(Right(7)))
Right(7)
*/
object EitherW {
  val demoes = List(
    // unary_~
    ("~Left(7)", ~Left(7)),
    ("~Right(7)", ~Right(7)),

    // ?
    ("Left(7) ? (\"foo\", \"bar\")", Left(7) ? ("foo", "bar")),
    ("Right(7) ? (\"foo\", \"bar\")", Right(7) ? ("foo", "bar")),

    // value
    ("left[Int](7).left value (_ + 1)", left[Int](7).left value (_ + 1)),
    ("left[Int](7).right value (_ + 1)", left[Int](7).right value (_ + 1)),
    ("right[Int](7).left value (_ + 1)", right[Int](7).left value (_ + 1)),
    ("right[Int](7).right value (_ + 1)", right[Int](7).right value (_ + 1)),

    // apply
    ("left[Int](7).left(Left((_: Int) + 1))", left[Int](7).left(Left((_: Int) + 1))),
    ("left[Int](7).left(Right((_: Int) + 1))", left[Int](7).left(Right((_: Int) + 1))),
    ("left[Int](7).right(Left((_: Int) + 1))", left[Int](7).right(Left((_: Int) + 1))),
    ("left[Int](7).right(Right((_: Int) + 1))", left[Int](7).right(Right((_: Int) + 1))),
    ("right[Int](7).left(Left((_: Int) + 1))", right[Int](7).left(Left((_: Int) + 1))),
    ("right[Int](7).left(Right((_: Int) + 1))", right[Int](7).left(Right((_: Int) + 1))),
    ("right[Int](7).right(Left((_: Int) + 1))", right[Int](7).right(Left((_: Int) + 1))),
    ("right[Int](7).right(Right((_: Int) + 1))", right[Int](7).right(Right((_: Int) + 1))),

    // joinLeft
    ("joinLeft(Left(Left(7)))", joinLeft(Left(Left(7)))),
    ("joinLeft(Left(Right(7)))", joinLeft(Left(Right(7)))),
    ("joinLeft(Right(Left(7)))", joinLeft(Right(Left(7)))),
    ("joinLeft(Right(Right(7)))", joinLeft(Right(Right(7)))),

    // joinRight
    ("joinRight(Left(Left(7)))", joinRight(Left(Left(7)))),
    ("joinRight(Left(Right(7)))", joinRight(Left(Right(7)))),
    ("joinRight(Right(Left(7)))", joinRight(Right(Left(7)))),
    ("joinRight(Right(Right(7)))", joinRight(Right(Right(7))))
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
