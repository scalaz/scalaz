package scalaz.example

import scalaz._

object ExampleEndo {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // An Endofunction is a function from of type (A => A).
    val inc: Endo[Int] = ((i: Int) => i + 1).endo

    // Two endofunctions can be combined through the Endo Monoid
    // This is equivalent to inc andThen inc.
    // The zero element of the Endo monoid is the identity function
    val inc2: Endo[Int] = inc ⊹ inc
    inc2(0) assert_≟ (2)
    val f = ∅[Endo[Int]]
    f(1) assert_≟ 1

    import scala.math._
    val capAndFloor: Endo[Int] = Seq(max(2, _: Int),  min(4, _: Int)).foldMap(_.endo)
    (0 to 5 toSeq) ∘ capAndFloor assert_≟ Seq(2, 2, 2, 3, 4, 4)
  }
}