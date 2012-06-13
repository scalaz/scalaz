package scalaz

class SyntaxTest extends Spec {
  "functor syntax" in {
    import syntax.functor._
    import std.tuple._, std.anyVal._

    ((1, 2) fmap (1 +) fmap (1 +)) must be_===((1, 4))
  }

  "functor syntax missing imports 1" in {
    import syntax.functor._
    //List(1) fmap (1 +) // uncomment to see the type error for missing type class instances
    ok
  }

  "functor syntax missing imports 2" in {
    import syntax.functor._
    //(1, 2) fmap (1 +) // uncomment to see the type error for missing type class instances, based on the @implicitNotFound annotation on scalaz.Unapply.
    ok
  }
}
