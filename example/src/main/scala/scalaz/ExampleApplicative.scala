package scalaz

object ExampleApplicative {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Apply the List Applicative functor
    ((List(40, 50, 60) ⊛ (List(1, 2, 3) ∘ ((_: Int) * (_: Int)).curry))) assert_≟ List(40, 50, 60, 80, 100, 120, 120, 150, 180)

    // Lift-2 the List Applicative functor
    List(1, 2, 3) <⊛> (List(40, 50, 60), (_: Int) * (_: Int)) assert_≟ List(40, 50, 60, 80, 100, 120, 120, 150, 180)

    // Lift-2 the List Applicative functor to a pair
    (List(1, 2, 3) <×> List(40, 50, 60)) assert_≟ List((1, 40), (1, 50), (1, 60), (2, 40), (2, 50), (2, 60), (3, 40), (3, 50), (3, 60))

    // Apply the List Applicative functor with an anonymous right
    (List(1, 2, 3) ⊛> List(40, 50, 60)) assert_≟ List(40, 50, 60, 40, 50, 60, 40, 50, 60)

    // Apply the List Applicative functor with an anonymous left
    (List(1, 2, 3) <⊛ List(40, 50, 60)) assert_≟ List(1, 1, 1, 2, 2, 2, 3, 3, 3)

    {
      // Apply the Function Applicative functor to produce a function that lifts conjunction
      // i.e. x => if(x < 15 && x % 2 == 0) without repeating the application to x.
      val z = ((_: Int) > 15) ⊛ (((_: Int) % 2 == 0) ∘ ((_: Boolean) ∧ (_: Boolean)).curry)
      
      List(7, 8, 14, 15, 16, 20, 21) ∘ z assert_≟ List(false,false,false,false,true,true,false)
    }
  }
}
