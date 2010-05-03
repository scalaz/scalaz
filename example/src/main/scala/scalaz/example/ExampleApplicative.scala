package scalaz.example

import scalaz._

object ExampleApplicative {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Apply the List Applicative functor
    ((List(40, 50, 60) <*> (List(1, 2, 3) ∘ ((_: Int) * (_: Int)).curried))) assert_≟ List(40, 50, 60, 80, 100, 120, 120, 150, 180)

    // Lift-2 the List Applicative functor
    List(1, 2, 3).<**>(List(40, 50, 60))(_ * _) assert_≟ List(40, 50, 60, 80, 100, 120, 120, 150, 180)

    // Lift-2 the List Applicative functor to a pair
    (List(1, 2, 3) <|*|> List(40, 50, 60)) assert_≟ List((1, 40), (1, 50), (1, 60), (2, 40), (2, 50), (2, 60), (3, 40), (3, 50), (3, 60))

    // Apply the List Applicative functor with an anonymous right
    (List(1, 2, 3) *> List(40, 50, 60)) assert_≟ List(40, 50, 60, 40, 50, 60, 40, 50, 60)

    // Apply the List Applicative functor with an anonymous left
    (List(1, 2, 3) <* List(40, 50, 60)) assert_≟ List(1, 1, 1, 2, 2, 2, 3, 3, 3)

    {
      // Apply the Function Applicative functor to produce a function that lifts conjunction
      // i.e. x => if(x < 15 && x % 2 == 0) without repeating the application to x.
      val z = ((_: Int) > 15) <*> (((_: Int) % 2 == 0) ∘ ((_: Boolean) ∧ (_: Boolean)).curried)
      
      List(7, 8, 14, 15, 16, 20, 21) ∘ z assert_≟ List(false,false,false,false,true,true,false)
    }

    def s[A](a: A) = a.success[List[String]]
    def f[A](s: String) = ff(List(s))
    def ff[A](s: List[String]) = s.fail[Int]
    val add3 = (x: Int) => (y: Int) => (z: Int) => x + y + z

    s(7) <*> (s(8) <*> (s(9) ∘ add3)) assert_≟ s(24)
    f("bzzt") <*> (s(8) <*> (f("bang") ∘ add3)) assert_≟ ff(List("bang", "bzzt"))

    s(7) *> s(8) assert_≟ s(8)
    s(7) *> f("bzzt") assert_≟ f("bzzt")
    f("bang") *> s(8) assert_≟ f("bang")
    f("bang") *> f("bzzt") assert_≟ ff(List("bang", "bzzt"))

    s(7) <* s(8) assert_≟ s(7)
    s(7) <* f("bzzt") assert_≟ f("bzzt")
    f("bang") <* s(8) assert_≟ f("bang")
    f("bang") <* f("bzzt") assert_≟ ff(List("bang", "bzzt"))

    s(7) <|*|> s(8) assert_≟ s(7, 8)
    s(7) <|*|> f("bzzt") assert_≟ List("bzzt").fail
    f("bang") <|*|> s(8) assert_≟ List("bang").fail
    f("bang") <|*|> f("bzzt") assert_≟ List("bang", "bzzt").fail

    // Using alternative syntax to directly apply a sequence of N applicative
    // arguments to a N-ary function.
    val a, b, c, d = List(1)
    (a ⊛ b){_ + _}
    a ⊛ b apply {_ + _}
    (a ⊛ b ⊛ c){_ + _ + _}
    a ⊛ b ⊛ c apply {_ + _ + _}
    a ⊛ b ⊛ c ⊛ d apply {_ + _ + _ + _}
    a |@| b |@| c |@| d apply {_ + _ + _ + _}

    (a ⊛ b ⊛ c ⊛ d).tupled
    (a |@| b |@| c |@| d).tupled

    case class Person(age: Int, name: String)
    some(10) ⊛ none[String] apply Person.apply
  }
}
