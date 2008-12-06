package scalazdemo.control

/*
(f > g)("7")
14.0

List(1, 2, 3) > add1
List(2, 3, 4)

Some(7) > add1
Some(8)

none[Int] > add1
None

left[Int, Int](7) > add1
Left(7)

right[String, Int](7) > add1
Right(8)

(g <-: f)("7")
14.0

add1 <-: List(1, 2, 3)
List(2, 3, 4)

add1 <-: Some(7)
Some(8)

add1 <-: none[Int]
None

add1 <-: left[Int, Int](7)
Left(7)

add1 <-: right[String, Int](7)
Right(8)
*/
object Functor {
  import scalaz.control.FunctorW.{EitherFunctor, Function1Functor, OptionFunctor, ListFunctor}
  import scalaz.OptionW.none
  import scalaz.EitherW.{left, right}

  val f = Integer.parseInt(_: String)
  val g = (x: Int) => x * 2F

  val add1 = (n: Int) => n + 1

  val demoes = List(
    // >
    ("(f > g)(\"7\")", (f > g)("7")),
    ("List(1, 2, 3) > add1", List(1, 2, 3) > add1),
    ("Some(7) > add1", Some(7) > add1),
    ("none[Int] > add1", none[Int] > add1),
    ("left[Int, Int](7) > add1", left[Int, Int](7) > add1),
    ("right[String, Int](7) > add1", right[String, Int](7) > add1),

    // <-:
    ("(g <-: f)(\"7\")", (g <-: f)("7")),
    ("add1 <-: List(1, 2, 3)", add1 <-: List(1, 2, 3)),
    ("add1 <-: Some(7)", add1 <-: Some(7)),
    ("add1 <-: none[Int]", add1 <-: none[Int]),
    ("add1 <-: left[Int, Int](7)", add1 <-: left[Int, Int](7)),
    ("add1 <-: right[String, Int](7)", add1 <-: right[String, Int](7))
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}