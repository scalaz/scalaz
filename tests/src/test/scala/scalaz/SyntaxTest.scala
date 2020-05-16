package scalaz

object SyntaxTest extends SpecLite {
  "functor syntax" in {
    import syntax.functor._
    import std.tuple._, std.anyVal._

    ((1, 2) ∘ (1 + _) ∘ (1 + _)) must_===((1, 4))
  }

  "functor syntax missing imports 1" in {
    import syntax.functor._
    //List(1) ∘ (1 + _) // uncomment to see the type error for missing type class instances
    ()
  }

  "functor syntax missing imports 2" in {
    import syntax.functor._
    //(1, 2) ∘ (1 + _) // uncomment to see the type error for missing type class instances, based on the @implicitNotFound annotation on scalaz.Unapply.
    ()
  }

  "show interpolator works with Strings" in {
    import syntax.show._
    import std.string._

    val x = "string"
    val resultString = s"""I've found a "$x""""
    z"I've found a $x" must_===(resultString)
  }

  "show interpolator works with case classes" in {
    import syntax.show._
    import std.string._

    case class TestFoo(x: String)
    implicit val fooShow : Show[TestFoo] = Show.showFromToString[TestFoo]

    val f = TestFoo("bar")
    val result = "I'd like a TestFoo(bar)"

    z"I'd like a $f" must_===(result)
  }


  "show interpolator works for coproduct sealed traits in their widened context" in {
    import syntax.show._
    import std.string._

    sealed trait CoproductT
    case object Result1T extends CoproductT
    case object Result2T extends CoproductT
    object CoproductT {
      implicit val coproductTShow : Show[CoproductT] = Show.show {
        case Result1T => Cord("Result1T")
        case Result2T => Cord("Result2T")
      }
    }

    val r : CoproductT = Result1T

    z"We have a $r" must_===("We have a Result1T")
  }
}
