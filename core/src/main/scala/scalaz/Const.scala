package scalaz

case class Const[A, B](getConst: A) {
  def ===(x: Const[A, B])(implicit A: Equal[A]): Boolean =
    A.equal(getConst, x.getConst)

  def map[C](f: B => C): Const[A, C] = Const(getConst)
}

object Const extends ConstInstances with ConstFunctions

sealed abstract class ConstInstances {
  /** The constant functor that maps every type to `A` */
  implicit def constFunctor[C]: Functor[({type l[a] = Const[C, a]})#l] =
    new Functor[({type l[a] = Const[C, a]})#l] {
      override def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] =
        fa.map(f)
    }

  implicit def constApplicative[F: Monoid] = new Applicative[({ type l[a] = Const[F, a] })#l] {
    def point[A](a: => A): Const[F, A] = Const(Monoid[F].zero)

    def ap[A, B](fa: => Const[F, A])(f: => Const[F, A => B]): Const[F, B] =
      Const(Monoid[F].append(fa.getConst, f.getConst))
  }

  implicit def constEqual[A : Equal, B]: Equal[Const[A, B]] =
    new Equal[Const[A, B]] {
      override def equal(a1: Const[A, B], a2: Const[A, B]): Boolean =
        a1 === a2
    }
}

sealed trait ConstFunctions {
  /** A properly universally quantified constant function. */
  def const[A](a: A): Function0 ~> ({type l[_] = A})#l =
    new (Function0 ~> ({type l[_] = A})#l) {
      override def apply[B](fa: Function0[B]): A = a
    }
}
