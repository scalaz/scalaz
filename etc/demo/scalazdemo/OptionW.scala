package scalazdemo

import scalaz.OptionW._

/*
Some(7).fold(42, _ + 1)
8

none[Int].fold(42, _ + 1)
42

Some(7) ? (8, 9)
9

none[Int] ? (8, 9)
8

Some(7) | 8
7

none[Int] | 8
8

Some(7) toNull
7

none[Int] toNull
null

onull(7)
Some(7)

onull(null.asInstanceOf[String])
None

onull(null.asInstanceOf[Int])
Some(0)

somes(List())
List()

somes(List(Some(7)))
List(7)

somes(List(None))
List()

somes(List(Some(7), None, Some(8), Some(9), None))
List(7, 8, 9)

join(Some(Some(7)))
Some(7)

join(Some(None))
None

join(None))
None

cond(true, 7)
Some(7)

cond(false, 7)
None
*/
object OptionW {
  val demoes = List(
    // fold
    ("Some(7).fold(42, _ + 1)", Some(7).fold(42, _ + 1)),
    ("none[Int].fold(42, _ + 1)", none[Int].fold(42, _ + 1)),

    // ?
    ("Some(7) ? (8, 9)", Some(7) ? (8, 9)),
    ("none[Int] ? (8, 9)", none[Int] ? (8, 9)),

    // |
    ("Some(7) | 8", Some(7) | 8),
    ("none[Int] | 8", none[Int] | 8),

    // toNull
    ("Some(7) toNull", Some(7) toNull),
    ("none[Int] toNull", none[Int] toNull),

    // onull
    ("onull(7)", onull(7)),
    ("onull(null.asInstanceOf[String])", onull(null.asInstanceOf[String])),
    ("onull(null.asInstanceOf[Int])", onull(null.asInstanceOf[Int])), // DANGER!

    // somes
    ("somes(List())", somes(List())),
    ("somes(List(Some(7)))", somes(List(Some(7)))),
    ("somes(List(None))", somes(List(None))),
    ("somes(List(Some(7), None, Some(8), Some(9), None))", somes(List(Some(7), None, Some(8), Some(9), None))),

    // join
    ("join(Some(Some(7)))", join(Some(Some(7)))),
    ("join(Some(None))", join(Some(None))),
    ("join(None))", join(None)),

    // cond
    ("cond(true, 7)", cond(true, 7)),
    ("cond(false, 7)", cond(false, 7))
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
