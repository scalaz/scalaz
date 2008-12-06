package scalazdemo.control

/*
some(7) <*> (none[Int] > add2)
None

some(7) <*> (some(8) > add2)
Some(15)

some(7) <*> (some("foo") <*> (some(List(1, 2, 3)) > ((ns: List[Int]) => (s: String) => (n: Int) => s :: (n :: ns).map(_.toString))))
Some(List(foo, 7, 1, 2, 3))

(add2 <-: some(7)) <*>: some(8)
Some(15)

(add2 <-: some(7)) <*>: none
None

some(7) *> some(8)
Some(8)

some(7) *> none[Int]
None

none[Int] *> some(8)
None

none[Int] *> none[Int]
None

some(7) <* some(8)
Some(7)

some(7) <* none[Int]
None

none[Int] <* some(8)
None

none[Int] <* none[Int]
None

some(7) <*|*> some(8)
Some((7,8))

some(7) <*|*> none[Int]
None

none[Int] <*|*> some(8)
None

none[Int] <*|*> none[Int]
None

List(1, 2, 3) <*> (List("a", "b") > ((s: String) => (n: Int) => n + s + n))
List(1a1, 2a2, 3a3, 1b1, 2b2, 3b3)

List(1, 2, 3) *> List("a", "b")
List(a, b, a, b, a, b)

List(1, 2, 3) <* List("a", "b")
List(1, 1, 2, 2, 3, 3)

List(1, 2, 3) <*|*> List("a", "b")
List((1,a), (1,b), (2,a), (2,b), (3,a), (3,b))

(((_: Int) + 1) <*> liftShowReverse)(12345)
64321

(((_: Int) + 13) <*> liftShowReverse)(12345)
85321

right[Int, Int](7) <*> (right[Int, Int](8) <*> (right[Int, Int](9) > add3))
Right(24)

left[Int, Int](7) <*> (right[Int, Int](8) <*> (left[Int, Int](9) > add3))
Left(9)

right[String, Int](7) *> right("foo")
Right(foo)

right[String, Int](7) *> left("foo")
Left(foo)

left[Int, String](7) *> right("foo")
Left(7)

left[Int, String](7) *> left(8)
Left(7)

right[String, Int](7) <* right("foo")
Right(7)

right[String, Int](7) <* left("foo")
Left(foo)

left[Int, String](7) <* right("foo")
Left(7)

left[Int, String](7) <* left(8)
Left(7)

right[String, Int](7) <*|*> right("foo")
Right((7,foo))

right[String, Int](7) <*|*> left("foo")
Left(foo)

left[Int, String](7) <*|*> right("foo")
Left(7)

left[Int, String](7) <*|*> left(8)
Left(7)

success[List[String], Int](7) <*> (success[List[String], Int](8) <*> (success[List[String], Int](9) > add3))
Success(24)

fail[List[String], Int](List("bad")) <*> (success[List[String], Int](8) <*> (fail[List[String], Int](List("error")) > add3))
Fail(List(error, bad))

success[List[String], Int](7) *> success(8)
Success(8)

success[List[String], Int](7) *> fail(List("foo"))
Fail(List(foo))

fail[List[String], Int](List("foo")) *> success(8)
Fail(List(foo))

fail[List[String], Int](List("foo")) *> fail(List("bar"))
Fail(List(foo, bar))

success[List[String], Int](7) <* success(8)
Success(7)

success[List[String], Int](7) <* fail(List("foo"))
Fail(List(foo))

fail[List[String], Int](List("foo")) <* success(8)
Fail(List(foo))

fail[List[String], Int](List("foo")) <* fail(List("bar"))
Fail(List(foo, bar))

success[List[String], Int](7) <*|*> success(8)
Success((7,8))

success[List[String], Int](7) <*|*> fail(List("foo"))
Fail(List(foo))

fail[List[String], Int](List("foo")) <*|*> success(8)
Fail(List(foo))

fail[List[String], Int](List("foo")) <*|*> fail(List("bar"))
Fail(List(foo, bar))
*/
object Applicative {
  import scalaz.control.ApplicativeW.{OptionApplicative, ListApplicative, Function1Applicative, EitherApplicative, ValidationApplicative}
  import scalaz.control.Pure._
  import scalaz.PartialType
  import scalaz.OptionW._
  import scalaz.EitherW._
  import scalaz.validation.Validation.{success, fail}

  val add2 = (x: Int) => (y: Int) => x + y
  val add3 = (x: Int) => (y: Int) => (z: Int) => x + y + z
  val liftShowReverse = pure[PartialType[Function1, Int]#Apply]((_: Int).toString.reverse.mkString)

  val demoes = List(
    // OptionApplicative
    // One or more None results in None, otherwise results in Some

    // <*>
    ("some(7) <*> (none[Int] > add2)",
            some(7) <*> (none[Int] > add2)),

    ("some(7) <*> (some(8) > add2)",
            some(7) <*> (some(8) > add2)),

    ("some(7) <*> (some(\"foo\") <*> (some(List(1, 2, 3)) > ((ns: List[Int]) => (s: String) => (n: Int) => s :: (n :: ns).map(_.toString))))",
            some(7) <*> (some("foo") <*> (some(List(1, 2, 3)) >
              ((ns: List[Int]) => (s: String) => (n: Int) => s :: (n :: ns).map(_.toString))))),

    // <*>:
    ("(add2 <-: some(7)) <*>: some(8)",
            (add2 <-: some(7)) <*>: some(8)),
    ("(add2 <-: some(7)) <*>: none",
            (add2 <-: some(7)) <*>: none),

    // *>
    ("some(7) *> some(8)", some(7) *> some(8)),
    ("some(7) *> none[Int]", some(7) *> none[Int]),
    ("none[Int] *> some(8)", none[Int] *> some(8)),
    ("none[Int] *> none[Int]", none[Int] *> none[Int]),

    // <*
    ("some(7) <* some(8)", some(7) <* some(8)),
    ("some(7) <* none[Int]", some(7) <* none[Int]),
    ("none[Int] <* some(8)", none[Int] <* some(8)),
    ("none[Int] <* none[Int]", none[Int] <* none[Int]),

    // <*|*>
    ("some(7) <*|*> some(8)", some(7) <*|*> some(8)),
    ("some(7) <*|*> none[Int]", some(7) <*|*> none[Int]),
    ("none[Int] <*|*> some(8)", none[Int] <*|*> some(8)),
    ("none[Int] <*|*> none[Int]", none[Int] <*|*> none[Int]),

    // ListApplicative

    // <*>
    ("List(1, 2, 3) <*> (List(\"a\", \"b\") > ((s: String) => (n: Int) => n + s + n))",
            List(1, 2, 3) <*> (List("a", "b") > ((s: String) => (n: Int) => n + s + n))),

    // *>
    ("List(1, 2, 3) *> List(\"a\", \"b\")", List(1, 2, 3) *> List("a", "b")),

    // <*
    ("List(1, 2, 3) <* List(\"a\", \"b\")", List(1, 2, 3) <* List("a", "b")),

    // <*|*>
    ("List(1, 2, 3) <*|*> List(\"a\", \"b\")", List(1, 2, 3) <*|*> List("a", "b")),

    // Function1Applicative
    ("(((_: Int) + 1) <*> liftShowReverse)(12345)", (((_: Int) + 1) <*> liftShowReverse)(12345)),
    ("(((_: Int) + 13) <*> liftShowReverse)(12345)", (((_: Int) + 13) <*> liftShowReverse)(12345)),

    // EitherApplicative
    // One or more Left results in Left, otherwise results in Right

    // <*>
    ("right[Int, Int](7) <*> (right[Int, Int](8) <*> (right[Int, Int](9) > add3))",
            right[Int, Int](7) <*> (right[Int, Int](8) <*> (right[Int, Int](9) > add3))),

    ("left[Int, Int](7) <*> (right[Int, Int](8) <*> (left[Int, Int](9) > add3))",
            left[Int, Int](7) <*> (right[Int, Int](8) <*> (left[Int, Int](9) > add3))),

    // *>
    ("right[String, Int](7) *> right(\"foo\")", right[String, Int](7) *> right("foo")),
    ("right[String, Int](7) *> left(\"foo\")", right[String, Int](7) *> left("foo")),
    ("left[Int, String](7) *> right(\"foo\")", left[Int, String](7) *> right("foo")),
    ("left[Int, String](7) *> left(8)", left[Int, String](7) *> left(8)),

    // <*
    ("right[String, Int](7) <* right(\"foo\")", right[String, Int](7) <* right("foo")),
    ("right[String, Int](7) <* left(\"foo\")", right[String, Int](7) <* left("foo")),
    ("left[Int, String](7) <* right(\"foo\")", left[Int, String](7) <* right("foo")),
    ("left[Int, String](7) <* left(8)", left[Int, String](7) <* left(8)),

    // <*|*>
    ("right[String, Int](7) <*|*> right(\"foo\")", right[String, Int](7) <*|*> right("foo")),
    ("right[String, Int](7) <*|*> left(\"foo\")", right[String, Int](7) <*|*> left("foo")),
    ("left[Int, String](7) <*|*> right(\"foo\")", left[Int, String](7) <*|*> right("foo")),
    ("left[Int, String](7) <*|*> left(8)", left[Int, String](7) <*|*> left(8)),

    // ValidationApplicative
    // If one or more Fail, then accumulate in Fail using the Semigroup, otherwise result in Success

    // <*>
    ("success[List[String], Int](7) <*> (success[List[String], Int](8) <*> (success[List[String], Int](9) > add3))",
            success[List[String], Int](7) <*> (success[List[String], Int](8) <*> (success[List[String], Int](9) > add3))),

    ("fail[List[String], Int](List(\"bad\")) <*> (success[List[String], Int](8) <*> (fail[List[String], Int](List(\"error\")) > add3))",
            fail[List[String], Int](List("bad")) <*> (success[List[String], Int](8) <*> (fail[List[String], Int](List("error")) > add3))),

    // *>
    ("success[List[String], Int](7) *> success(8)", success[List[String], Int](7) *> success(8)),
    ("success[List[String], Int](7) *> fail(List(\"foo\"))", success[List[String], Int](7) *> fail(List("foo"))),
    ("fail[List[String], Int](List(\"foo\")) *> success(8)", fail[List[String], Int](List("foo")) *> success(8)),
    ("fail[List[String], Int](List(\"foo\")) *> fail(List(\"bar\"))", fail[List[String], Int](List("foo")) *> fail(List("bar"))),

    // <*
    ("success[List[String], Int](7) <* success(8)", success[List[String], Int](7) <* success(8)),
    ("success[List[String], Int](7) <* fail(List(\"foo\"))", success[List[String], Int](7) <* fail(List("foo"))),
    ("fail[List[String], Int](List(\"foo\")) <* success(8)", fail[List[String], Int](List("foo")) <* success(8)),
    ("fail[List[String], Int](List(\"foo\")) <* fail(List(\"bar\"))", fail[List[String], Int](List("foo")) <* fail(List("bar"))),

    // <*|*>
    ("success[List[String], Int](7) <*|*> success(8)", success[List[String], Int](7) <*|*> success(8)),
    ("success[List[String], Int](7) <*|*> fail(List(\"foo\"))", success[List[String], Int](7) <*|*> fail(List("foo"))),
    ("fail[List[String], Int](List(\"foo\")) <*|*> success(8)", fail[List[String], Int](List("foo")) <*|*> success(8)),
    ("fail[List[String], Int](List(\"foo\")) <*|*> fail(List(\"bar\"))", fail[List[String], Int](List("foo")) <*|*> fail(List("bar")))
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
