package scalaz
package syntax
package std

trait Function1V[T, R] extends SyntaxV[T => R] {
  def on[X](f: (R, R) => X, t1: T, t2: T): X = f(self(t1), self(t2))

  import NonEmptyList._
  import Validation._

  def arrow[A[_, _]](implicit a: Arrow[A]): A[T, R] =
    a.arr(self)

  def kleisli[Z[_]](implicit z: Pointed[Z]): Kleisli[Z, T, R] =
    Kleisli.kleisli((t: T) => z.point(self(t)))

  def unary_!(implicit m: Memo[T, R]): (T) => R = m(self)

  def toValidation[E](e: => E)(implicit ev: R =:= Boolean): T => Validation[NonEmptyList[E], T] =
    (t: T) => (if (self(t): Boolean) success(t) else failure(nel(e, Nil)))

  def byName: (=> T) => R = t => self(t)

  def endo(implicit ev: R =:= T): Endo[T] =
    Endo.endo(t => ev(self(t)))

  def comparing(implicit o: Order[R]): (T, T) => Ordering =
    (t1, t2) => o.order(self(t1), self(t2))

  def equaling(implicit e: Equal[R]): (T, T) => Boolean =
    (t1, t2) => e.equal(self(t1), self(t2))

  def succState(implicit e: Enum[T]): State[T, R] =
    e.succState(self)

  def succStateZeroM[Y](k: R => State[T, Y])(implicit e: Enum[T], m: Monoid[T]): Y =
    e.succStateZeroM(self, k)

  def succStateZero[Y](k: R => Y)(implicit e: Enum[T], m: Monoid[T]): Y =
    e.succStateZero(self, k)

  def succStateMinM[Y](k: R => State[T, Y])(implicit e: Enum[T]): Option[Y] =
    e.succStateMinM(self, k)

  def succStateMin[Y](k: R => Y)(implicit e: Enum[T]): Option[Y] =
    e.succStateMin(self, k)

  def predState(implicit e: Enum[T]): State[T, R] =
    e.predState(self)

  def predStateZeroM[Y](k: R => State[T, Y])(implicit e: Enum[T], m: Monoid[T]): Y =
    e.predStateZeroM(self, k)

  def predStateZero[Y](k: R => Y)(implicit e: Enum[T], m: Monoid[T]): Y =
    e.predStateZero(self, k)

  def predStateMaxM[Y](k: R => State[T, Y])(implicit e: Enum[T]): Option[Y] =
    e.predStateMaxM(self, k)

  def predStateMax[Y](k: R => Y)(implicit e: Enum[T]): Option[Y] =
    e.predStateMax(self, k)

}
