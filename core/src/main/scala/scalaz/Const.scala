package scalaz

final case class Const[A, B](getConst: A)

private sealed trait ConstFunctor[C] extends Functor[({type λ[α] = Const[C, α]})#λ]{
  override def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] = Const(fa.getConst)
}

private sealed trait ConstApply[C] extends Apply[({type λ[α] = Const[C, α]})#λ] with ConstFunctor[C]{
  def C: Monoid[C]

  override def ap[A, B](fa: => Const[C, A])(f: => Const[C, A => B]): Const[C, B] =
    Const(C.append(fa.getConst, f.getConst))
}

private sealed trait ConstApplicative[C] extends Applicative[({type λ[α] = Const[C, α]})#λ] with ConstApply[C]{
  override def point[A](a: => A): Const[C, A] = Const(C.zero)
}

private sealed trait ConstEqual[A, B] extends Equal[Const[A, B]] {
  def OA: Equal[A]

  override def equal(a1: Const[A, B], a2: Const[A, B]): Boolean =
    OA.equal(a1.getConst, a2.getConst)
}

sealed trait ConstInstances3 {
  implicit def constFunctor[C]: Functor[({type λ[α] = Const[C, α]})#λ] = new ConstFunctor[C]{}
}

sealed trait ConstInstances2 extends ConstInstances3 {
  implicit def constApply[C: Monoid]: Apply[({type λ[α] = Const[C, α]})#λ] = new ConstApply[C] {
    val C: Monoid[C] = implicitly
  }
}


sealed trait ConstInstances1 extends ConstInstances2 {
  implicit def constApplicative[C: Monoid]: Applicative[({type λ[α] = Const[C, α]})#λ] = new ConstApplicative[C] {
    val C: Monoid[C] = implicitly
  }
}

sealed abstract class ConstInstances0 extends ConstInstances1 {
  implicit def constEqual[A : Equal, B]: Equal[Const[A, B]] = new ConstEqual[A, B]{
    val OA: Equal[A] = implicitly
  }
}

sealed abstract class ConstInstances extends ConstInstances0

object Const extends ConstInstances with ConstFunctions


sealed trait ConstFunctions {
  /** A properly universally quantified constant function. */
  def const[A](a: A): Function0 ~> ({type l[_] = A})#l =
    new (Function0 ~> ({type l[_] = A})#l) {
      override def apply[B](fa: Function0[B]): A = a
    }
}
