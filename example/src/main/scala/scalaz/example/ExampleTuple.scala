package scalaz.example

import scalaz._


object ExampleTuple {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val t1 = (1, 2, 3)

    // Folding a TupleN with a FunctionN to a single value
    t1 fold (_ + _ + _) assert_≟ 6

    // Copy a tuple, changing the value and possibly the type of some elements.
    //
    // copy accepts as parameters N values that are used to generate a new TupleN.
    // The parameters are named identically to the member methods of TupleN, and default to the
    // previous value of the element.
    //
    // This is identical to the copy method that is automatically added to case classes since Scala 2.8.
    t1 copy (_1 = 0) assert_≟ (0, 2, 3)

    // Map functions over the elements of a tuple to generate a new tuple.
    //
    // mapElements accepts as parameters N functions that are applied to respective elements of the TupleN to generate
    // a new Tuple. The parameters are named identically to the member methods of TupleN, and default to the
    // identity function.
    t1 mapElements (_ * 2, _ * 3, _ * 4) assert_≟ (2, 6, 12)
    t1 mapElements (_3 = "x" * _) assert_≟ (1, 2, "xxx")

    // Convert a TupleN to an IndexedSeq.
    val t1Seq = t1.toIndexedSeq
    t1Seq assert_≟ IndexedSeq(1, 2, 3)

    // The Least Upper Bound of the tuple elements is preserved as the the static type of the IndexedSeq
    // unlike Product#productIterator
    val t2 = (1, false)
    val t3 = (1, "two", false)

    val t2Seq = t2.toIndexedSeq
    val t3Seq = t3.toIndexedSeq

    implicitly[t1Seq.type <:< IndexedSeq[Int]]
    implicitly[t2Seq.type <:< IndexedSeq[AnyVal]]
    implicitly[t3Seq.type <:< IndexedSeq[Any]]
  }
}