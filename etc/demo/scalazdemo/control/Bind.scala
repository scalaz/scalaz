package scalazdemo.control

/*
join[Option](Some(Some(7)))
Some(7)

join[Option](Some(None))
None

join[Option](None)
None

join[List](List(List(1, 2, 3), List(4, 5, 6)))
List(1, 2, 3, 4, 5, 6)

join[EitherLeft](Right(Right(7)))
Right(7)

join[EitherLeft](Right(Left("abc")))
Left(abc)

join[EitherLeft](Left("abc"))
Left(abc)
*/
object Bind {
  import scalaz.PartialType
  import scalaz.control.Bind._

  type EitherLeft[X] = PartialType[Either, String]#Apply[X]

  val demoes = List(
    // join
    ("join[Option](Some(Some(7)))", join[Option](Some(Some(7)))),
    ("join[Option](Some(None))", join[Option](Some(None))),
    ("join[Option](None)", join[Option](None)),
    ("join[List](List(List(1, 2, 3), List(4, 5, 6)))", join[List](List(List(1, 2, 3), List(4, 5, 6)))),
    ("join[EitherLeft](Right(Right(7)))", join[EitherLeft](Right(Right(7)))),
    ("join[EitherLeft](Right(Left(\"abc\")))", join[EitherLeft](Right(Left("abc")))),
    ("join[EitherLeft](Left(\"abc\"))", join[EitherLeft](Left("abc")))
  )
  
  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
