package scalaz.control

import fjs.test.Property._
import fjs.test.Arbitrary
import EqualW._

object MonadPlusLaws {
  def associative[M[_], X](implicit m: MonadPlus[M], amx: Arbitrary[M[X]], e: Equal[M[X]]) =
    prop((mx1: M[X], mx2: M[X], mx3: M[X]) => m.plus(mx1, m.plus(mx2, mx3)) === m.plus(m.plus(mx1, mx2), mx3))
}
