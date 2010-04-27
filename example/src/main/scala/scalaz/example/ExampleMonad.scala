package scalaz.example

import scalaz._

object ExampleMonad {
  def main(args: Array[String]) = run

  import Scalaz._
  import collection.mutable.ArraySeq

  def run {
    // zeroOr
    some(7).orEmpty[List] assert_≟ List(7)
    none[Int].orEmpty[List] assert_≟ List()
    some(7).orEmpty[ArraySeq] assert_≟ ArraySeq(7)
    some(7).orEmpty[Option] assert_≟ some(7)

    // guard
    true.guard[List](7) assert_≟ List(7)
    false.guard[List](7) assert_≟ List()
    true.guard[Option](7) assert_≟ Some(7)
    false.guard[Option](7) assert_≟ None

    // prevent 
    true.prevent[List](8) assert_≟ List()
    false.prevent[List](8) assert_≟ List(8)
    true.prevent[Option](8) assert_≟ None
    false.prevent[Option](8) assert_≟ Some(8)

    // join μ
    (some(some(9)) μ) assert_≟ some(9)
    (some(none[Int]) μ) assert_≟ none[Int]
    (none[Option[Int]] μ) assert_≟ none[Int]
    (List(List(1, 2, 3), List(4, 5, 6)) μ) assert_≟ List(1, 2, 3, 4, 5, 6)

    // bind ∗
    (List(1, 2, 3) ∗ (List(7, _))) assert_≟ List(7, 1, 7, 2, 7, 3)
    (some(7) ∗ (x => if (x % 2 == 0) some(x - 1) else none)) assert_≟ none[Int]
    (some(8) ∗ (x => if (x % 2 == 0) some(x - 1) else none)) assert_≟ some(7)

    // anonymous bind ∗|
    (List(1, 2, 3) ∗| (List(3, 4))) assert_≟ List(3, 4, 3, 4, 3, 4)

    // Folding left on a List through the List monad
    List(1, 2).foldLeftM(0)((b, a) => List(10, 20, a, b)) assert_≟ List(10, 20, 2, 10, 10, 20, 2, 20, 10, 20, 2, 1, 10, 20, 2, 0)

    // Folding left on a Stream through the Option monad
    Stream(1, 2).foldLeftM(0)((b, a) => some(a + b)) assert_≟ some(3)

    // Folding right on a Stream through the List monad
    Stream(1, 2).foldRightM(0)((b, a) => List(10, 20, a, b)) assert_≟ List(10, 20, 1, 10, 10, 20, 1, 20, 10, 20, 1, 2, 10, 20, 1, 0)

    // Folding right on a List through the Option monad
    List(1, 2).foldRightM(0)((b, a) => some(a + b)) assert_≟ some(3)

    // Take-while on a List through the Option monad
    List.range(1, 50).takeWhileM(n => if (n < 100) some(n < 10) else none) assert_≟ some(List(1, 2, 3, 4, 5, 6, 7, 8, 9))

    // Take-while on a List through the List monad
    List.range(1, 15).takeWhileM(n => if (n < 5) List(n < 5, n < 2) else List(n % 5 > 0)) assert_≟ List(List(1, 2, 3, 4), List(1, 2, 3), List(1, 2), List(1), List(1, 2, 3, 4), List(1, 2, 3), List(1, 2), List(1))

    // Filtering on a List through the List monad (produces the powerset)
    List(1, 2, 3).filterM(_ => List(true, false)) assert_≟ List(List(1, 2, 3), List(1, 2), List(1, 3), List(1), List(2, 3), List(2), List(3), List())

    // Filtering on a List through the Option monad
    List(1, 2, 3).filterM(n => some(n < 3)) assert_≟ some(List(1, 2))

    // Replicating a List through the List monad
    List(1, 2, 3).replicateM[List](2) assert_≟ List(List(1, 1), List(1, 2), List(1, 3), List(2, 1), List(2, 2), List(2, 3), List(3, 1), List(3, 2), List(3, 3))

    // Replicating a List through the FirstOption monoid
    List(1, 2, 3).replicateM[FirstOption](2) assert_≟ List(some(1), some(1), some(1), some(2), some(2), some(2), some(3), some(3), some(3)).map(_.fst)

    // Replicating a List through the Option monoid
    List(1, 2, 3).replicateM[Option](2) assert_≟ List(some(2), some(3), some(4), some(3), some(4), some(5), some(4), some(5), some(6))

    // Replicating an Option through the ArraySeq monoid
    some(7).replicateM[ArraySeq](3) assert_≟ some(ArraySeq(7, 7, 7))

    // Replicating an Option through the Option monoid
    some(7).replicateM[Option](3) assert_≟ some(some(21))

    // Replicating an Option through the FisrtOption monoid
    some(7).replicateM[FirstOption](3) assert_≟ some(some(7).fst)
  }
}
