package scalaz.test

sealed trait Property {
  val gen: Gen[Status]
}

object Property {
  def property(g: Gen[Status]) = new Property {
    val gen = g
  }

  implicit def BooleanProperty(b: Boolean): Property = (if(b) Status.proven(Nil) else Status.falsified(Nil)).property

  def property[A](f: A => Property)(implicit aa: Arbitrary[A], sa: Shrink[A]): Property = aa.gen.forall(sa, Testable.testable(f))
}
