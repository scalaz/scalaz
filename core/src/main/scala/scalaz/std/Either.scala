package scalaz
package std

import scala.Either.{LeftProjection, RightProjection}
import scalaz.Isomorphism._
import scalaz.Tags.{First, Last}

trait EitherInstances0 {
  implicit def eitherEqual[A: Equal, B: Equal]: Equal[Either[A, B]] = new Equal[Either[A, B]] {
    def equal(f1: Either[A, B], f2: Either[A, B]) = (f1, f2) match {
      case (Left(a1), Left(a2)) => Equal[A].equal(a1, a2)
      case (Right(b1), Right(b2)) => Equal[B].equal(b1, b2)
      case (Right(_), Left(_)) | (Left(_), Right(_)) => false
    }
  }

  implicit def eitherLeftEqual[A: Equal, X]: Equal[LeftProjection[A, X]] = new Equal[LeftProjection[A, X]] {
    def equal(f1: LeftProjection[A, X], f2: LeftProjection[A, X]) = (f1.toOption, f2.toOption) match {
      case (Some(x), Some(y)) => Equal[A].equal(x, y)
      case (None, None) => true
      case _ => false
    }
  }

  implicit def eitherRightEqual[X, A: Equal]: Equal[RightProjection[X, A]] = new Equal[RightProjection[X, A]] {
    def equal(f1: RightProjection[X, A], f2: RightProjection[X, A]) = (f1.toOption, f2.toOption) match {
      case (Some(x), Some(y)) => Equal[A].equal(x, y)
      case (None, None) => true
      case _ => false
    }
  }

  implicit def eitherFirstRightEqual[X, A: Equal]: Equal[RightProjection[X, A] @@ First] = new Equal[RightProjection[X, A] @@ First] {
    def equal(a1: RightProjection[X, A] @@ First, a2: RightProjection[X, A] @@ First) =
      Equal[RightProjection[X, A]].equal(a1, a2)
  }

  implicit def eitherLastRightEqual[X, A: Equal]: Equal[RightProjection[X, A] @@ Last] = new Equal[RightProjection[X, A] @@ Last] {
    def equal(a1: RightProjection[X, A] @@ Last, a2: RightProjection[X, A] @@ Last) =
      Equal[RightProjection[X, A]].equal(a1, a2)
  }

  implicit def eitherFirstLeftEqual[A: Equal, X]: Equal[LeftProjection[A, X] @@ First] = new Equal[LeftProjection[A, X] @@ First] {
    def equal(a1: LeftProjection[A, X] @@ First, a2: LeftProjection[A, X] @@ First) =
      Equal[LeftProjection[A, X]].equal(a1, a2)
  }

  implicit def eitherLastLeftEqual[A: Equal, X]: Equal[LeftProjection[A, X] @@ Last] = new Equal[LeftProjection[A, X] @@ Last] {
    def equal(a1: LeftProjection[A, X] @@ Last, a2: LeftProjection[A, X] @@ Last) =
      Equal[LeftProjection[A, X]].equal(a1, a2)
  }

  implicit def eitherFirstLeftSemigroup[A: Semigroup, X]: Semigroup[LeftProjection[A, X] @@ First] = new Semigroup[LeftProjection[A, X] @@ First] {
    def append(f1: LeftProjection[A, X] @@ First, f2: => LeftProjection[A, X] @@ First) = if (f1.e.isLeft) f1 else f2
  }

  implicit def eitherFirstRightSemigroup[X, A: Semigroup]: Semigroup[RightProjection[X, A] @@ First] = new Semigroup[RightProjection[X, A] @@ First] {
    def append(f1: RightProjection[X, A] @@ First, f2: => RightProjection[X, A] @@ First) = if (f1.e.isRight) f1 else f2
  }

  implicit def eitherLastLeftSemigroup[A: Semigroup, X]: Semigroup[LeftProjection[A, X] @@ Last] = new Semigroup[LeftProjection[A, X] @@ Last] {
    def append(f1: LeftProjection[A, X] @@ Last, f2: => LeftProjection[A, X] @@ Last) = if (f1.e.isLeft) f1 else f2
  }

  implicit def eitherLastRightSemigroup[X, A: Semigroup]: Semigroup[RightProjection[X, A] @@ Last] = new Semigroup[RightProjection[X, A] @@ Last] {
    def append(f1: RightProjection[X, A] @@ Last, f2: => RightProjection[X, A] @@ Last) = if (f1.e.isRight) f1 else f2
  }

  implicit def eitherLeftSemigroup[A: Semigroup, X: Monoid]: Semigroup[LeftProjection[A, X]] = new Semigroup[LeftProjection[A, X]] {
    def append(f1: LeftProjection[A, X], f2: => LeftProjection[A, X]) = (f1.toOption, f2.toOption) match {
      case (Some(x), Some(y)) => Left(Semigroup[A].append(x, y)).left
      case (None, Some(_)) => f2
      case (Some(_), None) => f1
      case (None, None) => Right(Monoid[X].zero).left
    }
  }

  implicit def eitherRightSemigroup[X: Monoid, A: Semigroup]: Semigroup[RightProjection[X, A]] = new Semigroup[RightProjection[X, A]] {
    def append(f1: RightProjection[X, A], f2: => RightProjection[X, A]) = (f1.toOption, f2.toOption) match {
      case (Some(x), Some(y)) => Right(Semigroup[A].append(x, y)).right
      case (None, Some(_)) => f2
      case (Some(_), None) => f1
      case (None, None) => Left(Monoid[X].zero).right
    }
  }

}

trait EitherInstances extends EitherInstances0 {
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
  implicit def eitherMonad[L] = new Traverse[({type l[a] = Either[L, a]})#l] with Monad[({type l[a] = Either[L, a]})#l] {
    def bind[A, B](fa: Either[L, A])(f: (A) => Either[L, B]): Either[L, B] = fa match {
      case Left(a)  => Left(a)
      case Right(b) => f(b)
    }

    def point[A](a: => A): Either[L, A] = Right(a)

    def traverseImpl[G[_] : Applicative, A, B](fa: Either[L, A])(f: (A) => G[B]): G[Either[L, B]] = fa match {
      case Left(x)  => Applicative[G].point(Left(x))
      case Right(x) => Applicative[G].map(f(x))(Right(_))
    }

    def foldRight[A, B](fa: Either[L, A], z: => B)(f: (A, => B) => B): B = fa match {
      case Left(_)  => z
      case Right(a) => f(a, z)
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

  implicit def eitherOrder[A: Order, B: Order] = new Order[Either[A, B]] {
    import Ordering._
    def order(f1: Either[A, B], f2: Either[A, B]) = (f1, f2) match {
      case (Right(x), Right(y)) => Order[B].order(x, y)
      case (Left(x), Left(y)) => Order[A].order(x, y)
      case (Left(_), Right(_)) => LT
      case (Right(_), Left(_)) => GT
    }
  }

  implicit def eitherLeftOrder[A: Order, X]: Order[LeftProjection[A, X]] = new Order[LeftProjection[A, X]] {
    import Ordering._
    def order(f1: LeftProjection[A, X], f2: LeftProjection[A, X]) = (f1.toOption, f2.toOption) match {
      case (Some(x), Some(y)) => Order[A].order(x, y)
      case (None, Some(_)) => LT
      case (Some(_), None) => GT
      case (None, None) => EQ
    }
  }

  implicit def eitherRightOrder[X, A: Order] = new Order[RightProjection[X, A]] {
    import Ordering._
    def order(f1: RightProjection[X, A], f2: RightProjection[X, A]) = (f1.toOption, f2.toOption) match {
      case (Some(x), Some(y)) => Order[A].order(x, y)
      case (None, Some(_)) => LT
      case (Some(_), None) => GT
      case (None, None) => EQ
    }
  }

  implicit def eitherFirstLeftMonoid[A: Monoid, X: Monoid]: Monoid[LeftProjection[A, X] @@ First] = new Monoid[LeftProjection[A, X] @@ First] {
    def append(f1: LeftProjection[A, X] @@ First, f2: => LeftProjection[A, X] @@ First): LeftProjection[A, X] @@ First =
      eitherFirstLeftSemigroup(Semigroup[A]).append(f1, f2)
    def zero: LeftProjection[A, X] @@ First =
      Tag(Right(Monoid[X].zero).left)
  }

  implicit def eitherFirstRightMonoid[X: Monoid, A: Monoid]: Monoid[RightProjection[X, A] @@ First] = new Monoid[RightProjection[X, A] @@ First] {
    def append(f1: RightProjection[X, A] @@ First, f2: => RightProjection[X, A] @@ First): RightProjection[X, A] @@ First =
      eitherFirstRightSemigroup(Semigroup[A]).append(f1, f2)
    def zero: RightProjection[X, A] @@ First =
      Tag(Left(Monoid[X].zero).right)
  }

  implicit def eitherLastLeftMonoid[A: Monoid, X: Monoid]: Monoid[LeftProjection[A, X] @@ Last] = new Monoid[LeftProjection[A, X] @@ Last] {
    def append(f1: LeftProjection[A, X] @@ Last, f2: => LeftProjection[A, X] @@ Last): LeftProjection[A, X] @@ Last =
      eitherLastLeftSemigroup(Semigroup[A]).append(f1, f2)
    def zero: LeftProjection[A, X] @@ Last =
      Tag(Right(Monoid[X].zero).left)
  }

  implicit def eitherLastRightMonoid[X: Monoid, A: Monoid]: Monoid[RightProjection[X, A] @@ Last] = new Monoid[RightProjection[X, A] @@ Last] {
    def append(f1: RightProjection[X, A] @@ Last, f2: => RightProjection[X, A] @@ Last): RightProjection[X, A] @@ Last =
      eitherLastRightSemigroup(Semigroup[A]).append(f1, f2)
    def zero: RightProjection[X, A] @@ Last =
      Tag(Left(Monoid[X].zero).right)
  }

  implicit def eitherLeftMonoid[A: Monoid, X: Monoid]: Monoid[LeftProjection[A, X]] = new Monoid[LeftProjection[A, X]] {
    def append(f1: LeftProjection[A, X], f2: => LeftProjection[A, X]) = eitherLeftSemigroup(Semigroup[A], Monoid[X]).append(f1, f2)
    def zero = Right(Monoid[X].zero).left
  }

  implicit def eitherRightMonoid[X: Monoid, A: Monoid]: Monoid[RightProjection[X, A]] = new Monoid[RightProjection[X, A]] {
    def append(f1: RightProjection[X, A], f2: => RightProjection[X, A]) = eitherRightSemigroup(Monoid[X], Semigroup[A]).append(f1, f2)
    def zero = Left(Monoid[X].zero).right
  }



  // TODO Semigroup(?), Show, ...
}

object either extends EitherInstances
