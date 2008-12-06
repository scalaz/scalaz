package scalazdemo.control

/*
sequence[Option, List, List](List(Some(7), Some(8), Some(9)))
Some(List(7, 8, 9))

sequence[Option, List, List](List(Some(7), None, Some(9)))
None

sequence[Option, List, Array](List(Some(7), Some(8), Some(9)))
Some(Array(7, 8, 9))

sequence[List, List, List](List(List(1, 2, 3), List(4, 5, 6)))
List(List(1, 4), List(1, 5), List(1, 6), List(2, 4), List(2, 5), List(2, 6), List(3, 4), List(3, 5), List(3, 6))

sequence[List, List, Option](List(List(1, 2, 3), List(4, 5, 6)))
List(Some(1), Some(1), Some(1), Some(2), Some(2), Some(2), Some(3), Some(3), Some(3))

sequence[EitherLeft, List, List](List(Right(7), Right(8), Right(9)))
Right(List(7, 8, 9))

sequence[EitherLeft, List, List](List(Right(7), Left("abc"), Right(9)))
Left(abc)

List("a", "b", "c").replicateM[List](3)
List(List(a, a, a), List(a, a, b), List(a, a, c), List(a, b, a), List(a, b, b), List(a, b, c), List(a, c, a), List(a, c, b), List(a, c, c), List(b, a, a), List(b, a, b), List(b, a, c), List(b, b, a), List(b, b, b), List(b, b, c), List(b, c, a), List(b, c, b), List(b, c, c), List(c, a, a), List(c, a, b), List(c, a, c), List(c, b, a), List(c, b, b), List(c, b, c), List(c, c, a), List(c, c, b), List(c, c, c))

List("a", "b", "c").replicateM[Option](3)
List(Some(a), Some(b), Some(c))

Some("a").replicateM[List](3)
Some(List(a, a, a))

none[String].replicateM[List](3)
None

none[String].replicateM[List](3)
None

Left("a").replicateM[List](3)
Left(a)

Right("b").replicateM[List](3)
Right(List(b, b, b))

List(1, 2, 3).mapM[List, List](i => (1 to i).toList)
List(List(1, 1, 1), List(1, 1, 2), List(1, 1, 3), List(1, 2, 1), List(1, 2, 2), List(1, 2, 3))

List(1, 2, 3).mapM[Array, List](i => (1 to i).toList)
List(Array(1, 1, 1), Array(1, 1, 2), Array(1, 1, 3), Array(1, 2, 1), Array(1, 2, 2), Array(1, 2, 3))

List(1, 2, 3).mapM[List, Option](Some(_))
Some(List(1, 2, 3))

List(1, 2, 3).mapM[List, Option](n => if(n == 4) None else Some(n))
Some(List(1, 2, 3))

List(1, 2, 3).mapM[List, Option](n => if(n == 3) None else Some(n))
None

*/
object Monad {
  import scalaz.PartialType
  import scalaz.control.Monad._
  import scalaz.OptionW.none
  import scalaz.control.MonadW.{OptionMonad, ListMonad, EitherMonad}

  type EitherLeft[X] = PartialType[Either, String]#Apply[X]

  val demoes = List(
    // sequence
    ("sequence[Option, List, List](List(Some(7), Some(8), Some(9)))", sequence[Option, List, List](List(Some(7), Some(8), Some(9)))),
    ("sequence[Option, List, List](List(Some(7), None, Some(9)))", sequence[Option, List, List](List(Some(7), None, Some(9)))),
    ("sequence[Option, List, Array](List(Some(7), Some(8), Some(9)))", sequence[Option, List, Array](List(Some(7), Some(8), Some(9)))),
    ("sequence[List, List, List](List(List(1, 2, 3), List(4, 5, 6)))", sequence[List, List, List](List(List(1, 2, 3), List(4, 5, 6)))),
    ("sequence[List, List, Option](List(List(1, 2, 3), List(4, 5, 6)))", sequence[List, List, Option](List(List(1, 2, 3), List(4, 5, 6)))),
    ("sequence[EitherLeft, List, List](List(Right(7), Right(8), Right(9)))", sequence[EitherLeft, List, List](List(Right(7), Right(8), Right(9)))),
    ("sequence[EitherLeft, List, List](List(Right(7), Left(\"abc\"), Right(9)))", sequence[EitherLeft, List, List](List(Right(7), Left("abc"), Right(9)))),

    // replicateM
    ("List(\"a\", \"b\", \"c\").replicateM[List](3)", List("a", "b", "c").replicateM[List](3)),
    ("List(\"a\", \"b\", \"c\").replicateM[Option](3)", List("a", "b", "c").replicateM[Option](3)),
    ("Some(\"a\").replicateM[List](3)", Some("a").replicateM[List](3)),
    ("none[String].replicateM[List](3)", none[String].replicateM[List](3)),
    ("none[String].replicateM[List](3)", none[String].replicateM[List](3)),
    ("Left(\"a\").replicateM[List](3)", Left("a").replicateM[List](3)),
    ("Right(\"b\").replicateM[List](3)", Right("b").replicateM[List](3)),

    // mapM
    ("List(1, 2, 3).mapM[List, List](i => (1 to i).toList)", List(1, 2, 3).mapM[List, List](i => (1 to i).toList)),
    ("List(1, 2, 3).mapM[Array, List](i => (1 to i).toList)", List(1, 2, 3).mapM[Array, List](i => (1 to i).toList)),
    ("List(1, 2, 3).mapM[List, Option](Some(_))", List(1, 2, 3).mapM[List, Option](Some(_))),
    ("List(1, 2, 3).mapM[List, Option](n => if(n == 4) None else Some(n))", List(1, 2, 3).mapM[List, Option](n => if(n == 4) None else Some(n))),
    ("List(1, 2, 3).mapM[List, Option](n => if(n == 3) None else Some(n))", List(1, 2, 3).mapM[List, Option](n => if(n == 3) None else Some(n)))
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }

}
