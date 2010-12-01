package scalaz

trait Distributes[F[_], G[_]] {
  def apply[A](f: F[G[A]]): G[F[A]]
}

/** A natural transformation beween functors F and G **/
trait ~>[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

/** A constrained natural transformation **/
trait Constrained[F[_], G[_], E[_]] {
  def apply[A: E](f: F[A]): G[A]
}

/** A transformation natural in both sides of a bifunctor **/
trait ~~>[F[_,_], G[_,_]] {
  def apply[A,B](f: F[A,B]): G[A,B]
}

/** A constrained transformation natural in both sides of a bifunctor **/
trait Biconstrained[F[_,_], G[_,_], C[_], E[_]] {
  def apply[A: C, B: E](f: F[A,B]): G[A,B]
}

trait Dinatural[F[_,_], G[_,_]] {
  def apply[A](f: F[A,A]): G[A,A]
}

trait Biff[P[_,_], F[_], G[_]] {
  type Apply[A, B] = P[F[A], G[B]]
}

trait On[P[_,_], F[_]] { 
  type Apply[A, B] = P[F[A], F[B]]
}

trait Extras {
  type Id[A] = A
  trait Konst[A] {
    type Apply[B] = A
  }
  type Thunk[A] = () => A

  /** A function type encoded as a natural transformation */
  type ->[A, B] = Konst[A]#Apply ~> Konst[B]#Apply

  /** A universally quantified identity function */
  def id = new (Id ~> Id) {
    def apply[A](a: A) = a
  }

  implicit def natToFunction[F[_], G[_], A](f: F ~> G): F[A] => G[A] = x => f(x)
}
