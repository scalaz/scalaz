package scalaz

import std.AllInstances._

object NonEmptyListTestJVM extends SpecLite {

  "NonEmptyList.foldRight1 large list" in {
    import NonEmptyList._
    import syntax.foldable1._
    nel(0, IList.fromList(List.fill(100000)(1))).foldRight1(_ + _) must_== 100000
  }
  "no stack overflow large list traverse" in {
    import syntax.traverse._
    val largeNel = NonEmptyList.nel(0, IList.fromList((1 to 100000).toList))
    (largeNel map Option.apply).sequence must_===(Option(largeNel))
  }
  "stack-safety of tails" in {
    val largeNel = NonEmptyList.nel(0, IList.fill(100000)(0))
    largeNel.tails.size must_== 100001
  }

}
