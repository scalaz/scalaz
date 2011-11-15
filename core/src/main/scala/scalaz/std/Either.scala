package scalaz
package std

import scala.Either.{LeftProjection, RightProjection}
import scalaz.Isomorphism._
import scalaz.Tags.{First, Last}

trait EitherInstances0 {
  implicit def eitherEqual[A, B](implicit EqA: Equal[A], EqB: Equal[B]): Equal[Either[A, B]] = new EitherEqual[A, B] {
    implicit def A: Equal[A] = EqA
    implicit def B: Equal[B] = EqB
  }

  implicit def eitherLeftEqual[A, X](implicit EqA: Equal[A]): Equal[LeftProjection[A, X]] = new EitherLeftEqual[A, X] {
    implicit def A: Equal[A] = EqA
  }

  implicit def eitherRightEqual[X, A](implicit EqA: Equal[A]): Equal[RightProjection[X, A]] = new EitherRightEqual[X, A] {
    implicit def A: Equal[A] = EqA
  }

  implicit def eitherFirstRightEqual[X, A](implicit EqA: Equal[A]): Equal[RightProjection[X, A] @@ First] = new EitherFirstRightEqual[X, A] {
    implicit def A: Equal[A] = EqA
  }

  implicit def eitherLastRightEqual[X, A](implicit EqA: Equal[A]): Equal[RightProjection[X, A] @@ Last] = new EitherLastRightEqual[X, A] {
    implicit def A: Equal[A] = EqA
  }

  implicit def eitherFirstLeftEqual[A, X](implicit EqA: Equal[A]): Equal[LeftProjection[A, X] @@ First] = new EitherFirstLeftEqual[A, X] {
    implicit def A: Equal[A] = EqA
  }

  implicit def eitherLastLeftEqual[A, X](implicit EqA: Equal[A]): Equal[LeftProjection[A, X] @@ Last] = new EitherLastLeftEqual[A, X] {
    implicit def A: Equal[A] = EqA
  }

  implicit def eitherFirstLeftSemigroup[A: Semigroup, X]: Semigroup[LeftProjection[A, X] @@ First] = new EitherFirstLeftSemigroup[A, X] {}

  implicit def eitherFirstRightSemigroup[X, A: Semigroup]: Semigroup[RightProjection[X, A] @@ First] = new EitherFirstRightSemigroup[X, A] {}

  implicit def eitherLastLeftSemigroup[A: Semigroup, X]: Semigroup[LeftProjection[A, X] @@ Last] = new EitherLastLeftSemigroup[A, X] {}

  implicit def eitherLastRightSemigroup[X, A: Semigroup]: Semigroup[RightProjection[X, A] @@ Last] = new EitherLastRightSemigroup[X, A] {}

  implicit def eitherLeftSemigroup[A, X](implicit SemigroupA: Semigroup[A], MonoidX: Monoid[X]): Semigroup[LeftProjection[A, X]] = new EitherLeftSemigroup[A, X] {
    implicit def A: Semigroup[A] = SemigroupA
    implicit def X: Monoid[X] = MonoidX
  }

  implicit def eitherRightSemigroup[X, A](implicit MonoidX: Monoid[X], SemigroupA: Semigroup[A]): Semigroup[RightProjection[X, A]] = new EitherRightSemigroup[X, A] {
    implicit def X: Monoid[X] = MonoidX
    implicit def A: Semigroup[A] = SemigroupA
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

  /** LeftProjection is isomorphic to Validation, when the type parameter `E` is partially applied. */
  implicit def FirstLeftProjectionEIso2[E] = new IsoFunctorTemplate[({type λ[α] = LeftProjection[E, α] @@ First})#λ, ({type λ[α] = Either[E, α]})#λ] {
    def to[A](fa: LeftProjection[E, A] @@ First) = fa.e
    def from[A](ga: Either[E, A]): LeftProjection[E, A] @@ First = Tag(ga.left)
  }

  /** LeftProjection is isomorphic to Validation, when the type parameter `E` is partially applied. */
  implicit def LastLeftProjectionEIso2[E] = new IsoFunctorTemplate[({type λ[α] = LeftProjection[E, α] @@ Last})#λ, ({type λ[α] = Either[E, α]})#λ] {
    def to[A](fa: LeftProjection[E, A] @@ Last) = fa.e
    def from[A](ga: Either[E, A]): LeftProjection[E, A] @@ Last = Tag(ga.left)
  }

  /** LeftProjection is isomorphic to Validation  */
  implicit def LeftProjectionIso2 = new IsoBiFunctorTemplate[LeftProjection, Either] {
    def to[A, B](fa: LeftProjection[A, B]) = fa.e
    def from[A, B](ga: Either[A, B]) = ga.left
  }

  /** LeftProjection is isomorphic to Validation  */
  implicit def FirstLeftProjectionIso2 = new IsoBiFunctorTemplate[({type λ[α, β]=LeftProjection[α, β] @@ First})#λ, Either] {
    def to[A, B](fa: LeftProjection[A, B] @@ First) = fa.e
    def from[A, B](ga: Either[A, B]): LeftProjection[A, B] @@ First = Tag(ga.left)
  }

  /** LeftProjection is isomorphic to Validation  */
  implicit def LastLeftProjectionIso2 = new IsoBiFunctorTemplate[({type λ[α, β]=LeftProjection[α, β] @@ Last})#λ, Either] {
    def to[A, B](fa: LeftProjection[A, B] @@ Last) = fa.e
    def from[A, B](ga: Either[A, B]): LeftProjection[A, B] @@ Last = Tag(ga.left)
  }

  /** RightProjection is isomorphic to Validation, when the type parameter `A` is partially applied. */
  implicit def RightProjectionAIso2[A] = new IsoFunctorTemplate[({type λ[α] = RightProjection[α, A]})#λ, ({type λ[α] = Either[α, A]})#λ] {
    def to[E](fa: RightProjection[E, A]) = fa.e
    def from[E](ga: Either[E, A]) = ga.right
  }

  /** RightProjection is isomorphic to Validation, when the type parameter `A` is partially applied. */
  implicit def FirstRightProjectionAIso2[A] = new IsoFunctorTemplate[({type λ[α] = RightProjection[α, A] @@ First})#λ, ({type λ[α] = Either[α, A]})#λ] {
    def to[E](fa: RightProjection[E, A] @@ First) = fa.e
    def from[E](ga: Either[E, A]): RightProjection[E, A] @@ First = Tag(ga.right)
  }

  /** RightProjection is isomorphic to Validation, when the type parameter `A` is partially applied. */
  implicit def LastRightProjectionAIso2[A] = new IsoFunctorTemplate[({type λ[α] = RightProjection[α, A] @@ Last})#λ, ({type λ[α] = Either[α, A]})#λ] {
    def to[E](fa: RightProjection[E, A] @@ Last) = fa.e
    def from[E](ga: Either[E, A]): RightProjection[E, A] @@ Last = Tag(ga.right)
  }

  /** RightProjection is isomorphic to Validation  */
  implicit def RightProjectionIso2 = new IsoBiFunctorTemplate[RightProjection, Either] {
    def to[A, B](fa: RightProjection[A, B]) = fa.e
    def from[A, B](ga: Either[A, B]) = ga.right
  }

  /** RightProjection is isomorphic to Validation  */
  implicit def FirstRightProjectionIso2 = new IsoBiFunctorTemplate[({type λ[α, β]=RightProjection[α, β] @@ First})#λ, Either] {
    def to[A, B](fa: RightProjection[A, B] @@ First) = fa.e
    def from[A, B](ga: Either[A, B]): RightProjection[A, B] @@ First = Tag(ga.right)
  }

  /** RightProjection is isomorphic to Validation  */
  implicit def LastRightProjectionIso2 = new IsoBiFunctorTemplate[({type λ[α, β]=RightProjection[α, β] @@ Last})#λ, Either] {
    def to[A, B](fa: RightProjection[A, B] @@ Last) = fa.e
    def from[A, B](ga: Either[A, B]): RightProjection[A, B] @@ Last = Tag(ga.right)
  }

  implicit def eitherLeftInstance = new IsomorphismBiFunctor[LeftProjection, Either] {
    def iso = LeftProjectionIso2
    implicit def G: BiFunctor[Either] = eitherInstance
  }

  implicit def eitherFirstLeftInstance = new IsomorphismBiFunctor[({type f[a, b]=LeftProjection[a,b] @@ First})#f, Either] {
    def iso = FirstLeftProjectionIso2
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

  implicit def eitherFirstRightLInstance[L] = new Monad[({type λ[α] = RightProjection[L, α] @@ First})#λ] {
    def point[A](a: => A): RightProjection[L, A] @@ First = Tag(Right(a).right)
    def bind[A, B](fa: RightProjection[L, A] @@ First)(f: (A) => RightProjection[L, B] @@ First): RightProjection[L, B] @@ First = Tag(
      fa.e match {
        case Left(a)  => Left(a).right
        case Right(b) => f(b)
      }
    )
  }

  implicit def eitherLastRightLInstance[L] = new Monad[({type λ[α] = RightProjection[L, α] @@ Last})#λ] {
    def point[A](a: => A): RightProjection[L, A] @@ Last = Tag(Right(a).right)
    def bind[A, B](fa: RightProjection[L, A] @@ Last)(f: (A) => RightProjection[L, B] @@ Last): RightProjection[L, B] @@ Last = Tag(
      fa.e match {
        case Left(a)  => Left(a).right
        case Right(b) => f(b)
      }
    )
  }

  implicit def eitherLeftRInstance[R] = new Monad[({type λ[α] = LeftProjection[α, R]})#λ] {
    def point[A](a: => A): LeftProjection[A, R] = Left(a).left
    def bind[A, B](fa: LeftProjection[A, R])(f: (A) => LeftProjection[B, R]): LeftProjection[B, R] = fa.e match {
      case Left(a)  => f(a)
      case Right(b) => Right(b).left
    }
  }

  implicit def eitherFirstLeftRInstance[R] = new Monad[({type λ[α] = LeftProjection[α, R] @@ First})#λ] {
    def point[A](a: => A): LeftProjection[A, R] @@ First = Tag(Left(a).left)
    def bind[A, B](fa: LeftProjection[A, R] @@ First)(f: (A) => LeftProjection[B, R] @@ First): LeftProjection[B, R] @@ First = Tag(
      fa.e match {
        case Left(a)  => f(a)
        case Right(b) => Right(b).left
      }
    )
  }

  implicit def eitherLastLeftRInstance[R] = new Monad[({type λ[α] = LeftProjection[α, R] @@ Last})#λ] {
    def point[A](a: => A): LeftProjection[A, R] @@ Last = Tag(Left(a).left)
    def bind[A, B](fa: LeftProjection[A, R] @@ Last)(f: (A) => LeftProjection[B, R] @@ Last): LeftProjection[B, R] @@ Last = Tag(
      fa.e match {
        case Left(a)  => f(a)
        case Right(b) => Right(b).left
      }
    )
  }

  implicit def eitherOrder[A, B](implicit OrderA: Order[A], OrderB: Order[B]): Order[Either[A, B]] = new EitherOrder[A, B] {
    implicit def A: Order[A] = OrderA
    implicit def B: Order[B] = OrderB
  }

  implicit def eitherLeftOrder[A, X](implicit OrderA: Order[A]): Order[LeftProjection[A, X]] = new EitherLeftOrder[A, X] {
    implicit def A: Order[A] = OrderA
  }

  implicit def eitherRightOrder[X, A](implicit OrderA: Order[A]): Order[RightProjection[X, A]] = new EitherRightOrder[X, A] {
    implicit def A = OrderA
  }

  implicit def eitherFirstLeftOrder[A, X](implicit OrderA: Order[A]): Order[LeftProjection[A, X] @@ First] = new EitherFirstLeftOrder[A, X] {
    implicit def A: Order[A] = OrderA
  }

  implicit def eitherFirstRightOrder[X, A](implicit OrderA: Order[A]): Order[RightProjection[X, A] @@ First] = new EitherFirstRightOrder[X, A] {
    implicit def A = OrderA
  }

  implicit def eitherLastLeftOrder[A, X](implicit OrderA: Order[A]): Order[LeftProjection[A, X] @@ Last] = new EitherLastLeftOrder[A, X] {
    implicit def A: Order[A] = OrderA
  }

  implicit def eitherLastRightOrder[X, A](implicit OrderA: Order[A]): Order[RightProjection[X, A] @@ Last] = new EitherLastRightOrder[X, A] {
    implicit def A = OrderA
  }

  implicit def eitherFirstLeftMonoid[A, X](implicit MonoidX: Monoid[X]): Monoid[LeftProjection[A, X] @@ First] = new EitherFirstLeftMonoid[A, X] {
    implicit def X: Monoid[X] = MonoidX
  }

  implicit def eitherFirstRightMonoid[X, A](implicit MonoidX: Monoid[X]): Monoid[RightProjection[X, A] @@ First] = new EitherFirstRightMonoid[X, A] {
    implicit def X: Monoid[X] = MonoidX
  }


  implicit def eitherLastLeftMonoid[A, X](implicit MonoidX: Monoid[X]): Monoid[LeftProjection[A, X] @@ Last] = new EitherLastLeftMonoid[A, X] {
    implicit def X: Monoid[X] = MonoidX
  }

  implicit def eitherLastRightMonoid[X, A](implicit MonoidX: Monoid[X]): Monoid[RightProjection[X, A] @@ Last] = new EitherLastRightMonoid[X, A] {
    implicit def X: Monoid[X] = MonoidX
  }

  implicit def eitherLeftMonoid[A, X](implicit MonoidA: Monoid[A], MonoidX: Monoid[X]): Monoid[LeftProjection[A, X]] = new EitherLeftMonoid[A, X] {
    implicit def A: Monoid[A] = MonoidA
    implicit def X: Monoid[X] = MonoidX
  }

  implicit def eitherRightMonoid[X, A](implicit MonoidX: Monoid[X], MonoidA: Monoid[A]): Monoid[RightProjection[X, A]] = new EitherRightMonoid[X, A] {
    implicit def X: Monoid[X] = MonoidX
    implicit def A: Monoid[A] = MonoidA
  }
}

object either extends EitherInstances


private[scalaz] trait EitherRightEqual[X, A] extends Equal[RightProjection[X, A]] {
  implicit def A: Equal[A]

  def equal(a1: RightProjection[X, A], a2: RightProjection[X, A]) = (a1.toOption, a2.toOption) match {
    case (Some(x), Some(y)) => A.equal(x, y)
    case (None, None) => true
    case _ => false
  }
}

private[scalaz] trait EitherLeftEqual[A, X] extends Equal[LeftProjection[A, X]] {
  implicit def A: Equal[A]

  def equal(a1: LeftProjection[A, X], a2: LeftProjection[A, X]) = (a1.toOption, a2.toOption) match {
    case (Some(x), Some(y)) => A.equal(x, y)
    case (None, None) => true
    case _ => false
  }
}

private[scalaz] trait EitherFirstRightEqual[X, A] extends Equal[RightProjection[X, A] @@ First] {
  implicit def A: Equal[A]

  def equal(a1: RightProjection[X, A] @@ First, a2: RightProjection[X, A] @@ First) = (a1.toOption, a2.toOption) match {
    case (Some(x), Some(y)) => A.equal(x, y)
    case (None, None) => true
    case _ => false
  }
}

private[scalaz] trait EitherFirstLeftEqual[A, X] extends Equal[LeftProjection[A, X] @@ First] {
  implicit def A: Equal[A]

  def equal(a1: LeftProjection[A, X] @@ First, a2: LeftProjection[A, X] @@ First) = (a1.toOption, a2.toOption) match {
    case (Some(x), Some(y)) => A.equal(x, y)
    case (None, None) => true
    case _ => false
  }
}


private[scalaz] trait EitherLastRightEqual[X, A] extends Equal[RightProjection[X, A] @@ Last] {
  implicit def A: Equal[A]

  def equal(a1: RightProjection[X, A] @@ Last, a2: RightProjection[X, A] @@ Last) = (a1.toOption, a2.toOption) match {
    case (Some(x), Some(y)) => A.equal(x, y)
    case (None, None) => true
    case _ => false
  }
}

private[scalaz] trait EitherLastLeftEqual[A, X] extends Equal[LeftProjection[A, X] @@ Last] {
  implicit def A: Equal[A]

  def equal(a1: LeftProjection[A, X] @@ Last, a2: LeftProjection[A, X] @@ Last) = (a1.toOption, a2.toOption) match {
    case (Some(x), Some(y)) => A.equal(x, y)
    case (None, None) => true
    case _ => false
  }
}

private[scalaz] trait EitherEqual[A, B] extends Equal[Either[A, B]] {
  implicit def A: Equal[A]
  implicit def B: Equal[B]

  def equal(f1: Either[A, B], f2: Either[A, B]) = (f1, f2) match {
    case (Left(a1), Left(a2)) => A.equal(a1, a2)
    case (Right(b1), Right(b2)) => B.equal(b1, b2)
    case (Right(_), Left(_)) | (Left(_), Right(_)) => false
  }
}

private[scalaz] trait EitherFirstLeftSemigroup[A, X] extends Semigroup[LeftProjection[A, X] @@ First] {
  def append(f1: LeftProjection[A, X] @@ First, f2: => LeftProjection[A, X] @@ First) = if (f1.e.isLeft) f1 else f2
}

private[scalaz] trait EitherFirstRightSemigroup[X, A] extends Semigroup[RightProjection[X, A] @@ First] {
  def append(f1: RightProjection[X, A] @@ First, f2: => RightProjection[X, A] @@ First) = if (f1.e.isRight) f1 else f2
}

private[scalaz] trait EitherLastLeftSemigroup[A, X] extends Semigroup[LeftProjection[A, X] @@ Last] {
  def append(f1: LeftProjection[A, X] @@ Last, f2: => LeftProjection[A, X] @@ Last) = if (f1.e.isLeft) f1 else f2
}

private[scalaz] trait EitherLastRightSemigroup[X, A] extends Semigroup[RightProjection[X, A] @@ Last] {
  def append(f1: RightProjection[X, A] @@ Last, f2: => RightProjection[X, A] @@ Last) = if (f1.e.isRight) f1 else f2
}

private[scalaz] trait EitherLeftSemigroup[A, X] extends Semigroup[LeftProjection[A, X]] {
  implicit def A: Semigroup[A]
  implicit def X: Monoid[X]

  def append(f1: LeftProjection[A, X], f2: => LeftProjection[A, X]) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => Left(Semigroup[A].append(x, y)).left
    case (None, Some(_)) => f2
    case (Some(_), None) => f1
    case (None, None) => Right(Monoid[X].zero).left
  }
}

private[scalaz] trait EitherRightSemigroup[X, A] extends Semigroup[RightProjection[X, A]] {
  implicit def X: Monoid[X]
  implicit def A: Semigroup[A]

  def append(f1: RightProjection[X, A], f2: => RightProjection[X, A]) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => Right(Semigroup[A].append(x, y)).right
    case (None, Some(_)) => f2
    case (Some(_), None) => f1
    case (None, None) => Left(Monoid[X].zero).right
  }
}


private[scalaz] trait EitherFirstLeftMonoid[A, X] extends EitherFirstLeftSemigroup[A, X] with Monoid[LeftProjection[A, X] @@ First] {
  implicit def X: Monoid[X]

  def zero: LeftProjection[A, X] @@ First = Tag(Right(Monoid[X].zero).left)
}

private[scalaz] trait EitherLastLeftMonoid[A, X] extends EitherLastLeftSemigroup[A, X] with Monoid[LeftProjection[A, X] @@ Last] {
  implicit def X: Monoid[X]

  def zero: LeftProjection[A, X] @@ Last = Tag(Right(Monoid[X].zero).left)
}

private[scalaz] trait EitherLeftMonoid[A, X] extends EitherLeftSemigroup[A, X] with Monoid[LeftProjection[A, X]] {
  implicit def X: Monoid[X]

  def zero: LeftProjection[A, X] = Right(Monoid[X].zero).left
}

private[scalaz] trait EitherFirstRightMonoid[X, A] extends EitherFirstRightSemigroup[X, A] with Monoid[RightProjection[X, A] @@ First] {
  implicit def X: Monoid[X]

  def zero: RightProjection[X, A] @@ First = Tag(Left(Monoid[X].zero).right)
}

private[scalaz] trait EitherLastRightMonoid[X, A] extends EitherLastRightSemigroup[X, A] with Monoid[RightProjection[X, A] @@ Last] {
  implicit def X: Monoid[X]

  def zero: RightProjection[X, A] @@ Last = Tag(Left(Monoid[X].zero).right)
}

private[scalaz] trait EitherRightMonoid[X, A] extends EitherRightSemigroup[X, A] with Monoid[RightProjection[X, A]] {
  implicit def X: Monoid[X]

  def zero: RightProjection[X, A] = Left(Monoid[X].zero).right
}

private[scalaz] trait EitherOrder[A, B] extends Order[Either[A, B]] {
  implicit def A: Order[A]
  implicit def B: Order[B]

  import Ordering._
  def order(f1: Either[A, B], f2: Either[A, B]) = (f1, f2) match {
    case (Left(x), Left(y)) => A.order(x, y)
    case (Right(x), Right(y)) => B.order(x, y)
    case (Left(_), Right(_)) => LT
    case (Right(_), Left(_)) => GT
  }
}

private[scalaz] trait EitherLeftOrder[A, X] extends Order[LeftProjection[A, X]] {
  implicit def A: Order[A]

  import Ordering._
  def order(f1: LeftProjection[A, X], f2: LeftProjection[A, X]) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => A.order(x, y)
    case (None, Some(_)) => LT
    case (Some(_), None) => GT
    case (None, None) => EQ
  }
}

private[scalaz] trait EitherRightOrder[X, A] extends Order[RightProjection[X, A]] {
  implicit def A: Order[A]

  import Ordering._
  def order(f1: RightProjection[X, A], f2: RightProjection[X, A]) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => A.order(x, y)
    case (None, Some(_)) => LT
    case (Some(_), None) => GT
    case (None, None) => EQ
  }
}

private[scalaz] trait EitherFirstLeftOrder[A, X] extends Order[LeftProjection[A, X] @@ First] {
  implicit def A: Order[A]

  import Ordering._
  def order(f1: LeftProjection[A, X] @@ First, f2: LeftProjection[A, X] @@ First) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => A.order(x, y)
    case (None, Some(_)) => LT
    case (Some(_), None) => GT
    case (None, None) => EQ
  }
}

private[scalaz] trait EitherFirstRightOrder[X, A] extends Order[RightProjection[X, A] @@ First] {
  implicit def A: Order[A]

  import Ordering._
  def order(f1: RightProjection[X, A] @@ First, f2: RightProjection[X, A] @@ First) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => A.order(x, y)
    case (None, Some(_)) => LT
    case (Some(_), None) => GT
    case (None, None) => EQ
  }
}


private[scalaz] trait EitherLastLeftOrder[A, X] extends Order[LeftProjection[A, X] @@ Last] {
  implicit def A: Order[A]

  import Ordering._
  def order(f1: LeftProjection[A, X] @@ Last, f2: LeftProjection[A, X] @@ Last) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => A.order(x, y)
    case (None, Some(_)) => LT
    case (Some(_), None) => GT
    case (None, None) => EQ
  }
}

private[scalaz] trait EitherLastRightOrder[X, A] extends Order[RightProjection[X, A] @@ Last] {
  implicit def A: Order[A]

  import Ordering._
  def order(f1: RightProjection[X, A] @@ Last, f2: RightProjection[X, A] @@ Last) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => A.order(x, y)
    case (None, Some(_)) => LT
    case (Some(_), None) => GT
    case (None, None) => EQ
  }
}
