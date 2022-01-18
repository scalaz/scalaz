package scalaz

trait UnapplyProduct[TC[_[_]], MA, MB] {
  type M[X]
  type A
  type B
  def TC: TC[M]
  type MA_ = MA
  def _1(ma: MA): M[A]
  def _2(mb: MB): M[B]
}

object UnapplyProduct {

  /** Fetch a well-typed `UnapplyProduct` for the given typeclass and types. */
  def apply[TC[_[_]], MA, MB](implicit U: UnapplyProduct[TC, MA, MB]): U.type {
    type M[A] = U.M[A]
    type A = U.A
    type B = U.B
  } = U
}
