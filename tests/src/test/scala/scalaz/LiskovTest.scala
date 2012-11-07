package scalaz

class LiskovTest extends Spec {

  trait Co1[+ _]

  trait In1[_]

  trait Contra1[- _]

  trait Co1_2[+A, B]

  trait Co2_2[A, B]

  trait In2[A, B]

  trait Contra1_2[-A, B]

  trait Contra2_2[A, -B]

  import Liskov._

  "apply" in {
    implicitly[String <~< AnyRef].apply(""): AnyRef
    ok
  }

  "lift" in {
    def foo[A, B](implicit ev: A <~< B) {
      Liskov.co[Co1, A, B](ev)
      Liskov.co[In1, A, B](ev)
      Liskov.contra[Contra1, A, B](ev)

      Liskov.co2[Co1_2, B, A, Unit](ev)
      Liskov.co2[Co2_2, B, A, Unit](ev)
      Liskov.co2[In2, B, A, Unit](ev)
      Liskov.contra1_2[Contra1_2, B, A, Unit](ev)
      Liskov.contra2_2[Contra2_2, B, Unit, A](ev)
    }

    foo[String, AnyRef]

    ok
  }
}
