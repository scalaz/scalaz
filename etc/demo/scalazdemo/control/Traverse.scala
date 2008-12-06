package scalazdemo.control

/*
some("789").traverse[List](f)
List(Some(7), Some(8), Some(9))

none[String].traverse[List](f)
List(None)

some("789").traverse[Option](g)
Some(Some(789))

some("xxx").traverse[Option](g)
None

none[String].traverse[Option](g)
Some(None)

some("789").traverse[EitherLeft](h)
Right(Some(789))

some("xxx").traverse[EitherLeft](h)
Left(java.lang.NumberFormatException: For input string: "xxx")

none[String].traverse[EitherLeft](h)
Right(None)

List[String]().traverse[List](f)
List(List())

List("123", "456", "789").traverse[List](f)
List(List(1, 4, 7), List(1, 4, 8), List(1, 4, 9), List(1, 5, 7), List(1, 5, 8), List(1, 5, 9), List(1, 6, 7), List(1, 6, 8), List(1, 6, 9), List(2, 4, 7), List(2, 4, 8), List(2, 4, 9), List(2, 5, 7), List(2, 5, 8), List(2, 5, 9), List(2, 6, 7), List(2, 6, 8), List(2, 6, 9), List(3, 4, 7), List(3, 4, 8), List(3, 4, 9), List(3, 5, 7), List(3, 5, 8), List(3, 5, 9), List(3, 6, 7), List(3, 6, 8), List(3, 6, 9))

List[String]().traverse[Option](g)
Some(List())

List("123", "xxx", "789").traverse[Option](g)
None

List("123", "456", "789").traverse[Option](g)
Some(List(123, 456, 789))

List[String]().traverse[EitherLeft](h)
Right(List())

List("123", "456", "789").traverse[EitherLeft](h)
Right(List(123, 456, 789))

List("123", "xxx", "789").traverse[EitherLeft](h)
Left(java.lang.NumberFormatException: For input string: "xxx")

left[String, Int]("789").traverse[List](n => (1 to n).toList)
List(Left(789))

right[Int, String]("789").traverse[List](f)
List(Right(7), Right(8), Right(9))

left[String, String]("789").traverse[Option](g)
Some(Left(789))

right[Int, String]("xxx").traverse[Option](g)
None

right[Int, String]("789").traverse[Option](g)
Some(Right(789))

left[String, String]("789").traverse[EitherLeft](h)
Right(Left(789))

right[Int, String]("xxx").traverse[EitherLeft](h)
Left(java.lang.NumberFormatException: For input string: "xxx")

right[Int, String]("789").traverse[EitherLeft](h)
Right(Right(789))

fail[String, Int]("789").traverse[List](i)
List(Fail(789))

success[Int, String]("789").traverse[List](f)
List(Success(7), Success(8), Success(9))

fail[String, String]("789").traverse[Option](g)
Some(Fail(789))

success[Int, String]("xxx").traverse[Option](g)
None

success[Int, String]("789").traverse[Option](g)
Some(Success(789))

fail[String, String]("789").traverse[EitherLeft](h)
Right(Fail(789))

success[Int, String]("xxx").traverse[EitherLeft](h)
Left(java.lang.NumberFormatException: For input string: "xxx")

success[Int, String]("789").traverse[EitherLeft](h)
Right(Success(789))

List(List(1, 2, 3), List(4, 5, 6)) ->>
List(1, 2, 3, 4, 5, 6)

List("abc", "def") ->>
abcdef

Some("abc") ->>
abc

none[Int] ->>
0

List(2, 6, 7, 8, 9) ->>
32

List(false, false, true, false, true) ->>
true
*/
object Traverse {
  import scalaz.control.TraverseW.{ListTraverse, OptionTraverse, EitherTraverse, ValidationTraverse}
  import scalaz.PartialType
  import scalaz.OptionW.{some, none}
  import scalaz.EitherW.{throws, left, right}  
  import scalaz.validation.Validation.{success, fail}
  import scalaz.control.Monoid.IntAdditionMonoid
  import scalaz.control.Monoid.BooleanDisjunctionMonoid

  val f = (_: String).map(_ - 48).toList
  val g = (s: String) => h(s).right.toOption
  val h = (s: String) => throws(Integer.parseInt(s))
  val i = (n: Int) => (1 to n).toList

  type EitherLeft[X] = PartialType[Either, Throwable]#Apply[X]

  val demoes = List(
    // OptionTraverse
    ("some(\"789\").traverse[List](f)", some("789").traverse[List](f)),
    ("none[String].traverse[List](f)", none[String].traverse[List](f)),
    ("some(\"789\").traverse[Option](g)", some("789").traverse[Option](g)),
    ("some(\"xxx\").traverse[Option](g)", some("xxx").traverse[Option](g)),
    ("none[String].traverse[Option](g)", none[String].traverse[Option](g)),
    ("some(\"789\").traverse[EitherLeft](h)", some("789").traverse[EitherLeft](h)),
    ("some(\"xxx\").traverse[EitherLeft](h)", some("xxx").traverse[EitherLeft](h)),
    ("none[String].traverse[EitherLeft](h)", none[String].traverse[EitherLeft](h)),

    // ListTraverse
    ("List[String]().traverse[List](f)", List[String]().traverse[List](f)),
    ("List(\"123\", \"456\", \"789\").traverse[List](f)", List("123", "456", "789").traverse[List](f)),
    ("List[String]().traverse[Option](g)", List[String]().traverse[Option](g)),
    ("List(\"123\", \"xxx\", \"789\").traverse[Option](g)", List("123", "xxx", "789").traverse[Option](g)),
    ("List(\"123\", \"456\", \"789\").traverse[Option](g)", List("123", "456", "789").traverse[Option](g)),
    ("List[String]().traverse[EitherLeft](h)", List[String]().traverse[EitherLeft](h)),
    ("List(\"123\", \"456\", \"789\").traverse[EitherLeft](h)", List("123", "456", "789").traverse[EitherLeft](h)),
    ("List(\"123\", \"xxx\", \"789\").traverse[EitherLeft](h)", List("123", "xxx", "789").traverse[EitherLeft](h)),

    // EitherTraverse
    ("left[String, Int](\"789\").traverse[List](n => (1 to n).toList)", left[String, Int]("789").traverse[List](i)),
    ("right[Int, String](\"789\").traverse[List](f)", right[Int, String]("789").traverse[List](f)),
    ("left[String, String](\"789\").traverse[Option](g)", left[String, String]("789").traverse[Option](g)),
    ("right[Int, String](\"xxx\").traverse[Option](g)", right[Int, String]("xxx").traverse[Option](g)),
    ("right[Int, String](\"789\").traverse[Option](g)", right[Int, String]("789").traverse[Option](g)),
    ("left[String, String](\"789\").traverse[EitherLeft](h)", left[String, String]("789").traverse[EitherLeft](h)),
    ("right[Int, String](\"xxx\").traverse[EitherLeft](h)", right[Int, String]("xxx").traverse[EitherLeft](h)),
    ("right[Int, String](\"789\").traverse[EitherLeft](h)", right[Int, String]("789").traverse[EitherLeft](h)),

    // ValidationTraverse
    ("fail[String, Int](\"789\").traverse[List](i)", fail[String, Int]("789").traverse[List](i)),
    ("success[Int, String](\"789\").traverse[List](f)", success[Int, String]("789").traverse[List](f)),
    ("fail[String, String](\"789\").traverse[Option](g)", fail[String, String]("789").traverse[Option](g)),
    ("success[Int, String](\"xxx\").traverse[Option](g)", success[Int, String]("xxx").traverse[Option](g)),
    ("success[Int, String](\"789\").traverse[Option](g)", success[Int, String]("789").traverse[Option](g)),
    ("fail[String, String](\"789\").traverse[EitherLeft](h)", fail[String, String]("789").traverse[EitherLeft](h)),
    ("success[Int, String](\"xxx\").traverse[EitherLeft](h)", success[Int, String]("xxx").traverse[EitherLeft](h)),
    ("success[Int, String](\"789\").traverse[EitherLeft](h)", success[Int, String]("789").traverse[EitherLeft](h)),

    // ->>
    ("List(List(1, 2, 3), List(4, 5, 6)) ->>", List(List(1, 2, 3), List(4, 5, 6)) ->>),
    ("List(\"abc\", \"def\") ->>", List("abc", "def") ->>),
    ("Some(\"abc\") ->>", Some("abc") ->>),
    ("none[Int] ->>", none[Int] ->>),
    // 0/+
    ("List(2, 6, 7, 8, 9) ->>", List(2, 6, 7, 8, 9) ->>),
    // true/||
    ("List(false, false, true, false, true) ->>", List(false, false, true, false, true) ->>)
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
