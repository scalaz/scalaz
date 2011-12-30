package scalaz

class SyntaxTest extends Spec {
  val resultToProp = () // shodow pesky Specs implicit view

  "functor syntax" in {
    import syntax.functor._
    import std.tuple._, std.anyVal._

    ((1, 2) ∘ (1 +) ∘ (1 +)) must be_===((1, 4))
  }

  "functor syntax missing imports" in {
    import syntax.functor._
    // (1, 2) ∘ (1 +) // uncomment to see the type error for missing type class instances, based on the @implicitNotFound annotation on scalaz.Unapply.
    ok
  }
}
