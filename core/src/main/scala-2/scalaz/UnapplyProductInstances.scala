package scalaz

abstract class UnapplyProductInstances {
  /**
   * This is a workaround that allows us to approximate multiple implicit
   * parameter sections (which Scala does not currently support). See this gist
   * by Miles Sabin for the original context:
   *
   *   https://gist.github.com/milessabin/cadd73b7756fe4097ca0
   *
   * The key idea is that we can use an intermediate type to capture the type
   * members of the two `Unapply` instances in such a way that we can refer to
   * them in the implicit parameter list.
   */
  case class SingletonOf[T, U <: { type A; type M[_] }](widen: T { type A = U#A; type M[x] = U#M[x] })

  object SingletonOf {
    implicit def mkSingletonOf[T <: { type A; type M[_] }](implicit t: T): SingletonOf[T, t.type] =
      SingletonOf(t)
  }

  import LeibnizF._

  implicit def unapply[TC[_[_]], MA0, MB0, U1 <: { type A; type M[_] }, U2 <: { type A; type M[_] }](implicit
    sU1: SingletonOf[Unapply[TC, MA0], U1],
    sU2: SingletonOf[Unapply[TC, MB0], U2],
    leibnizF: U2#M =~= U1#M
  ): UnapplyProduct[TC, MA0, MB0] {
    type M[x] = U1#M[x]
    type A = U1#A
    type B = U2#A
  } = new UnapplyProduct[TC, MA0, MB0] {
    type M[x] = U1#M[x]
    type A = U1#A
    type B = U2#A
    def TC = sU1.widen.TC
    def _1(ma: MA0) = sU1.widen(ma)
    def _2(mb: MB0) = leibnizF(sU2.widen(mb))
  }
}
