package scalaz.example

import scalaz._

object ExampleArrow {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val plus1 = (_: Int) + 1
    val times2 = (_: Int) * 2
    val rev = (_: String) reverse

    // Function1 arrow
    {
      // Applying first on the Function1 arrow.
      plus1.first apply (7, "abc") assert_≟ (8, "abc")

      // Applying second on the Function1 arrow.
      plus1.second apply ("def", 14) assert_≟ ("def", 15)

      // Combine plus1 and rev on the Function1 arrow to apply across both elements of a pair.
      plus1 *** rev apply (7, "abc") assert_≟ (8, "cba")

      // Perform both plus1 and times2 on a value using the Function1 arrow
      plus1 &&& times2 apply 7 assert_≟ (8, 14)

      // Perform plus1 on a pair using the Function1 arrow
      plus1.product apply (9, 99) assert_≟ (10, 100)
    }
    
    // Kleisli arrow
    {
      val k = ☆((n: List[Int]) => if(n.isEmpty) None else Some(n ∘ (_.shows.reverse)))
      val s = ☆((n: Int) => if(n % 7 == 0) None else Some(n * 4))
      val t = ☆((n: Int) => if(n > 100) None else Some(n * 13))

      // Applying first on the Kleisli arrow using the Option monad.
      k.first apply ((44 to 49).toList, "abc") assert_≟ (List("44","54","64","74","84","94"), "abc").some
      k.first apply (Nil, "abc") assert_≟ None

      // Applying second on the Kleisli arrow using the Option monad.
      k.second apply ("abc", (44 to 49).toList) assert_≟ ("abc", List("44","54","64","74","84","94")).some
      k.second apply ("abc", Nil) assert_≟ None

      // Combine k and s on the Kleisli arrow using the Option monad.
      val p = k *** s
      p apply ((44 to 49).toList, 18) assert_≟ (List("44","54","64","74","84","94"), 72).some
      p apply ((44 to 49).toList, 14) assert_≟ None
      p apply (Nil, 18) assert_≟ None

      // Perform both s and t on a value on the Kleisli arrow using the Option monad.
      val q = s &&& t
      q apply 3 assert_≟ (12, 39).some
      q apply 7 assert_≟ None
      q apply 90 assert_≟ (360, 1170).some
      q apply 91 assert_≟ None
      q apply 92 assert_≟ (368, 1196).some
      q apply 104 assert_≟ None

      // Perform k on a pair on the Kleisli arrow using the Option monad.
      val j = k.product
      j apply ((44 to 49).toList, (12 to 18).toList) assert_≟ (List("44","54","64","74","84","94"),List("21", "31", "41", "51", "61", "71", "81")).some
      j apply (Nil, (12 to 18).toList) assert_≟ None
      j apply ((44 to 49).toList,Nil) assert_≟ None
    }

    // Cokleisli Arrow
    {
      val nums = nel(1, 2, 3)
      nums.υ assert_≟ nel(nel(1, 2, 3), nel(2, 3), nel(3))
      val sum = ★((m: NonEmptyList[Int]) => m.∑)
      val min = ★((m: NonEmptyList[Int]) => m.min)
      // todo this causes StackOverflowError.
//      ((sum &&& max) apply nums) assert_≟ nel1((some(6), some(1)), (some(5), some(2), (some(3), some(3))
    }

    List(1, 2, 3, 4)
  }
}
