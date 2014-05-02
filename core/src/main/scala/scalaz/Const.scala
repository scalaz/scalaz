package scalaz

final case class Const[A, B](getConst: A)

private sealed trait ConstFunctor[C] extends Functor[({type λ[α] = Const[C, α]})#λ]{
  override def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] = Const(fa.getConst)
}

private sealed trait ConstApply[C] extends Apply[({type λ[α] = Const[C, α]})#λ] with ConstFunctor[C]{
  def C: Semigroup[C]

  override def ap[A, B](fa: => Const[C, A])(f: => Const[C, A => B]): Const[C, B] =
    Const(C.append(f.getConst, fa.getConst))
}

private sealed trait ConstApplicative[C] extends Applicative[({type λ[α] = Const[C, α]})#λ] with ConstApply[C]{
  def C: Monoid[C]

  override def point[A](a: => A): Const[C, A] = Const(C.zero)
}

private sealed trait ConstEqual[A, B] extends Equal[Const[A, B]] {
  def OA: Equal[A]

  override def equal(a1: Const[A, B], a2: Const[A, B]): Boolean =
    OA.equal(a1.getConst, a2.getConst)

  override def equalIsNatural = OA.equalIsNatural
}

private sealed trait ConstOrder[A, B] extends Order[Const[A, B]] with ConstEqual[A, B] {
  def OA: Order[A]

  override def order(a1: Const[A, B], a2: Const[A, B]): Ordering =
    OA.order(a1.getConst, a2.getConst)
}

sealed abstract class ConstInstances1 {
  implicit def constFunctor[C]: Functor[({type λ[α] = Const[C, α]})#λ] = new ConstFunctor[C]{}
}

sealed abstract class ConstInstances0 extends ConstInstances1 {
  implicit def constEqual[A : Equal, B]: Equal[Const[A, B]] = new ConstEqual[A, B]{
    val OA: Equal[A] = implicitly
  }

  implicit def constApply[C: Semigroup]: Apply[({type λ[α] = Const[C, α]})#λ] = new ConstApply[C] {
    val C: Semigroup[C] = implicitly
  }
}

sealed abstract class ConstInstances extends ConstInstances0 {
  implicit def constOrder[A : Order, B]: Order[Const[A, B]] = new ConstOrder[A, B]{
    val OA: Order[A] = implicitly
  }

  implicit def constApplicative[C: Monoid]: Applicative[({type λ[α] = Const[C, α]})#λ] = new ConstApplicative[C] {
    val C: Monoid[C] = implicitly
  }
}

object Const extends ConstInstances with ConstFunctions


sealed trait ConstFunctions {
  /** A properly universally quantified constant function. */
  def const[A](a: A): Function0 ~> ({type l[_] = A})#l =
    new (Function0 ~> ({type l[_] = A})#l) {
      override def apply[B](fa: Function0[B]): A = a
    }
}
