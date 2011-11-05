package scalaz
package std

import scala.Either.{LeftProjection, RightProjection}
import scalaz.Isomorphism._

trait EitherInstances {
  implicit def eitherInstance = new BiTraverse[Either] {
    override def bimap[A, B, C, D](fab: Either[A, B])
                                  (f: (A) => C, g: (B) => D): Either[C, D] = fab match {
      case Left(a)  => Left(f(a))
      case Right(b) => Right(g(b))
    }

    def bitraverse[G[_] : Applicative, A, B, C, D](fab: Either[A, B])
                                                  (f: (A) => G[C], g: (B) => G[D]): G[Either[C, D]] = fab match {
      case Left(a)  => Applicative[G].map(f(a))(b => Left(b))
      case Right(b) => Applicative[G].map(g(b))(d => Right(d))
    }
  }

  /** Right biased monad */
  implicit def eitherMonad[L] = new Monad[({type l[a] = Either[L, a]})#l] with Traverse[({type l[a] = Either[L, a]})#l] {
    def bind[A, B](fa: Either[L, A])(f: (A) => Either[L, B]): Either[L, B] = fa match {
      case Left(a)  => Left(a)
      case Right(b) => f(b)
    }

    def point[A](a: => A): Either[L, A] = Right(a)

    def traverseImpl[G[_] : Applicative, A, B](fa: Either[L, A])(f: (A) => G[B]): G[Either[L, B]] = fa match {
      case Left(x)  => Applicative[G].point(Left(x))
      case Right(x) => Applicative[G].map(f(x))(Right(_))
    }

    def foldR[A, B](fa: Either[L, A], z: B)(f: (A) => (=> B) => B): B = fa match {
      case Left(_)  => z
      case Right(a) => f(a)(z)
    }
  }

  /** LeftProjection is isomorphic to Validation, when the type parameter `E` is partially applied. */
  implicit def LeftProjectionEIso2[E] = new IsoFunctorTemplate[({type λ[α] = LeftProjection[E, α]})#λ, ({type λ[α] = Either[E, α]})#λ] {
    def to[A](fa: LeftProjection[E, A]) = fa.e
    def from[A](ga: Either[E, A]) = ga.left
  }

  /** LeftProjection is isomorphic to Validation  */
  implicit def LeftProjectionIso2 = new IsoBiFunctorTemplate[LeftProjection, Either] {
    def to[A, B](fa: LeftProjection[A, B]) = fa.e
    def from[A, B](ga: Either[A, B]) = ga.left
  }

  /** RightProjection is isomorphic to Validation, when the type parameter `A` is partially applied. */
  implicit def RightProjectionAIso2[A] = new IsoFunctorTemplate[({type λ[α] = RightProjection[α, A]})#λ, ({type λ[α] = Either[α, A]})#λ] {
    def to[E](fa: RightProjection[E, A]) = fa.e
    def from[E](ga: Either[E, A]) = ga.right
  }

  /** RightProjection is isomorphic to Validation  */
  implicit def RightProjectionIso2 = new IsoBiFunctorTemplate[RightProjection, Either] {
    def to[A, B](fa: RightProjection[A, B]) = fa.e
    def from[A, B](ga: Either[A, B]) = ga.right
  }

  implicit def eitherLeftInstance = new IsomorphismBiFunctor[LeftProjection, Either] {
    def iso = LeftProjectionIso2
    implicit def G: BiFunctor[Either] = eitherInstance
  }

  implicit def eitherRightInstance = new IsomorphismBiFunctor[RightProjection, Either] {
    def iso = RightProjectionIso2
    implicit def G: BiFunctor[Either] = eitherInstance
  }

  implicit def eitherRightLInstance[L] = new Monad[({type λ[α] = RightProjection[L, α]})#λ] {
    def point[A](a: => A): RightProjection[L, A] = Right(a).right
    def bind[A, B](fa: RightProjection[L, A])(f: (A) => RightProjection[L, B]): RightProjection[L, B] = fa.e match {
      case Left(a)  => Left(a).right
      case Right(b) => f(b)
    }
  }

  implicit def eitherLeftRInstance[R] = new Monad[({type λ[α] = LeftProjection[α, R]})#λ] {
    def point[A](a: => A): LeftProjection[A, R] = Left(a).left
    def bind[A, B](fa: LeftProjection[A, R])(f: (A) => LeftProjection[B, R]): LeftProjection[B, R] = fa.e match {
      case Left(a)  => f(a)
      case Right(b) => Right(b).left
    }
  }

  // TODO Semigroup(?), Show, ...
}

object either extends EitherInstances
