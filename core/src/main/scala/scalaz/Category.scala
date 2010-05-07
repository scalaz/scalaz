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

  /** Every monoid gives rise to a category **/
  implicit def MonoidCategory[M: Monoid] = new Category[PartialApply1Of3[Const2,M]#Apply] {
    def id[A] = Const2(implicitly[Zero[M]].zero)
    def compose[X, Y, Z](f: Const2[M, Y, Z], g: Const2[M, X, Y]) = Const2(f.value |+| g.value)
  }

  implicit def ObjectToMorphism[A,B,C](a: A): Const2[A,Unit,Unit] = Const2(a)
  implicit def MorphismToObject[A,B,C](a: Const2[A,B,C]) = a.value

  case class Discrete[X, A, B](value: X => X) extends NewType[X => X]

  implicit def DiscreteCategory[X] = new Category[PartialApply1Of3[Discrete,X]#Apply] {
    def id[A] = Discrete(x => x)
    def compose[A,B,C](f: Discrete[X, B, C], g: Discrete[X, A, B]) = Discrete(f.value compose g.value)
  }
}
