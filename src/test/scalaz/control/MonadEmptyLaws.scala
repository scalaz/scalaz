package scalaz.control

import fjs.test.Property._
import fjs.test.Arbitrary
import EqualW._

object MonadEmptyLaws {
  def leftEmptyIdentity[M[_], X, Y](implicit m: MonadEmpty[M], axmy: Arbitrary[X => M[Y]], e: Equal[M[Y]]) =
    prop((f: X => M[Y]) => m.empty[Y] === m.bind(f, m.empty[X]))
  
  def rightEmptyIdentity[M[_], X](implicit m: MonadEmpty[M], amx: Arbitrary[M[X]], e: Equal[M[X]]) =
    prop((mx: M[X]) => m.empty[X] === m.bind((x: X) => mx, m.empty[X]))
}
