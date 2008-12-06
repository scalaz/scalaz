package scalaz.control

import fjs.test.Property._
import fjs.test.Arbitrary
import EqualW._

object MonoidLaws {
  def leftIdentity[A](implicit m: Monoid[A], aa: Arbitrary[A], e: Equal[A]) =
    prop((a: A) => m.append(m.zero, a) === a)

  def rightIdentity[A](implicit m: Monoid[A], aa: Arbitrary[A], e: Equal[A]) =
    prop((a: A) => m.append(a, m.zero) === a)    
}
