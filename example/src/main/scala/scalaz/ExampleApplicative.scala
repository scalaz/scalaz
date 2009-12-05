package scalaz

object ExampleApplicative {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Applicative functor apply
    ((List(40, 50, 60) ⊛ (List(1, 2, 3) ∘ ((_: Int) * (_: Int)).curry))) assert_≟ List(40, 50, 60, 80, 100, 120, 120, 150, 180)

    // Applicative functor lift
    List(1, 2, 3) <⊛> (List(40, 50, 60), (_: Int) * (_: Int)) assert_≟ List(40, 50, 60, 80, 100, 120, 120, 150, 180)

    // Applicative functor lift to pair
    (List(1, 2, 3) <×> List(40, 50, 60)) assert_≟ List((1, 40), (1, 50), (1, 60), (2, 40), (2, 50), (2, 60), (3, 40), (3, 50), (3, 60))

    // Applicative functor lift (anonymous right)
    (List(1, 2, 3) ⊛> List(40, 50, 60)) assert_≟ List(40, 50, 60, 40, 50, 60, 40, 50, 60)

    // Applicative functor lift (anonymous left)
    (List(1, 2, 3) <⊛ List(40, 50, 60)) assert_≟ List(1, 1, 1, 2, 2, 2, 3, 3, 3)
  }
}