package scalaz

trait Category[C[_, _]] {
  def id[A]: C[A, A]
  def compose[X, Y, Z](f: C[Y, Z], g: C[X, Y]): C[X, Z]
}

object Category {
  import Scalaz._
  
  /** The <b>Set</b> category **/
  implicit def Function1Category: Category[Function1] = new Category[Function1] {
    def id[A] = a => a
    def compose[X, Y, Z](f: Y => Z, g: X => Y) = f compose g   
  }

  implicit def KleisliCategory[M[_]: Monad]: Category[PartialApplyK[Kleisli, M]#Apply] = new Category[PartialApplyK[Kleisli, M]#Apply] {
    def id[A] = ☆(_ η)
    def compose[X, Y, Z](f: Kleisli[M, Y, Z], g: Kleisli[M, X, Y]) = f <=< g
  }

  implicit def CokleisliCategory[M[_]: Comonad]: Category[PartialApplyK[Cokleisli, M]#Apply] = new Category[PartialApplyK[Cokleisli, M]#Apply] {
    def id[A] = ★(_ ε)
    def compose[X, Y, Z](f: Cokleisli[M, Y, Z], g: Cokleisli[M, X, Y]) = f =<= g 
  }

  type Morphism[A,B,C] = A

  /** Every monoid gives rise to a category **/
  implicit def MonoidToCategory[M: Monoid] = new Category[PartialApply1Of3[Morphism,M]#Apply] {
    def id[A] = mempty
    def compose[X, Y, Z](f: M, g: M) = f |+| g
  }

  implicit def ObjectToMorphism[A](a: A): PartialApply1Of3[Morphism,A]#Apply[Unit,Unit] = a
}
