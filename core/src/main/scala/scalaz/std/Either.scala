package scalaz
package std

import scala.Either.{LeftProjection, RightProjection}
import scalaz.Isomorphism._
import scalaz.Tags.{First, Last}

sealed trait EitherInstances0 {
  implicit def eitherEqual[A, B](implicit A0: Equal[A], B0: Equal[B]): Equal[Either[A, B]] = new EitherEqual[A, B] {
    implicit def A = A0
    implicit def B = B0
  }

  implicit def eitherLeftEqual[A, X](implicit A0: Equal[A]): Equal[LeftProjection[A, X]] = new EitherLeftEqual[A, X] {
    implicit def A = A0
  }

  implicit def eitherRightEqual[X, A](implicit A0: Equal[A]): Equal[RightProjection[X, A]] = new EitherRightEqual[X, A] {
    implicit def A = A0
  }

  implicit def eitherFirstRightEqual[X, A](implicit A0: Equal[A]): Equal[RightProjection[X, A] @@ First] = First.subst(Equal[RightProjection[X, A]])

  implicit def eitherLastRightEqual[X, A](implicit A0: Equal[A]): Equal[RightProjection[X, A] @@ Last] = Last.subst(Equal[RightProjection[X, A]])

  implicit def eitherFirstLeftEqual[A, X](implicit A0: Equal[A]): Equal[LeftProjection[A, X] @@ First] = First.subst(Equal[LeftProjection[A, X]])

  implicit def eitherLastLeftEqual[A, X](implicit A0: Equal[A]): Equal[LeftProjection[A, X] @@ Last] = Last.subst(Equal[LeftProjection[A, X]])

  implicit def eitherFirstLeftSemigroup[A: Semigroup, X]: Semigroup[LeftProjection[A, X] @@ First] = new EitherFirstLeftSemigroup[A, X] {}

  implicit def eitherFirstRightSemigroup[X, A: Semigroup]: Semigroup[RightProjection[X, A] @@ First] = new EitherFirstRightSemigroup[X, A] {}

  implicit def eitherLastLeftSemigroup[A: Semigroup, X]: Semigroup[LeftProjection[A, X] @@ Last] = new EitherLastLeftSemigroup[A, X] {}

  implicit def eitherLastRightSemigroup[X, A: Semigroup]: Semigroup[RightProjection[X, A] @@ Last] = new EitherLastRightSemigroup[X, A] {}

  implicit def eitherLeftSemigroup[A, X](implicit SemigroupA: Semigroup[A], MonoidX: Monoid[X]): Semigroup[LeftProjection[A, X]] = new EitherLeftSemigroup[A, X] {
    implicit def A = SemigroupA
    implicit def X = MonoidX
  }

  implicit def eitherRightSemigroup[X, A](implicit MonoidX: Monoid[X], SemigroupA: Semigroup[A]): Semigroup[RightProjection[X, A]] = new EitherRightSemigroup[X, A] {
    implicit def X = MonoidX
    implicit def A = SemigroupA
  }

}

trait EitherInstances extends EitherInstances0 {
  implicit val eitherInstance = new Bitraverse[Either] {
    override def bimap[A, B, C, D](fab: Either[A, B])
                                  (f: A => C, g: B => D) = fab match {
      case Left(a)  => Left(f(a))
      case Right(b) => Right(g(b))
    }

    def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: Either[A, B])
                                                  (f: A => G[C], g: B => G[D]) = fab match {
      case Left(a)  => Applicative[G].map(f(a))(b => Left(b))
      case Right(b) => Applicative[G].map(g(b))(d => Right(d))
    }
  }

  /** Right biased monad */
  implicit def eitherMonad[L]: Traverse[Either[L, ?]] with MonadError[Either[L, ?], L] with BindRec[Either[L, ?]] with Cozip[Either[L, ?]] =
    new Traverse[Either[L, ?]] with MonadError[Either[L, ?], L] with BindRec[Either[L, ?]] with Cozip[Either[L, ?]] {
      def bind[A, B](fa: Either[L, A])(f: A => Either[L, B]) = fa match {
        case Left(a)  => Left(a)
        case Right(b) => f(b)
      }

      def handleError[A](fa: Either[L, A])(f: L => Either[L, A]) =
        fa match {
          case a @ Right(_) => a
          case Left(a) => f(a)
        }

      def raiseError[A](e: L) =
        Left(e)

      def point[A](a: => A) = Right(a)

      def traverseImpl[G[_] : Applicative, A, B](fa: Either[L, A])(f: A => G[B]) = fa match {
        case Left(x)  => Applicative[G].point(Left(x))
        case Right(x) => Applicative[G].map(f(x))(Right(_))
      }

      override def foldRight[A, B](fa: Either[L, A], z: => B)(f: (A, => B) => B) = fa match {
        case Left(_)  => z
        case Right(a) => f(a, z)
      }

      def cozip[A, B](a: Either[L, A \/ B]) =
        a match {
          case Left(l) => -\/(Left(l))
          case Right(e)=> e match {
            case -\/(a) => -\/(Right(a))
            case \/-(b) => \/-(Right(b))
          }
        }

      @scala.annotation.tailrec
      def tailrecM[A, B](f: A => Either[L, A \/ B])(a: A): Either[L, B] =
        f(a) match {
          case Left(l) => Left(l)
          case Right(-\/(a)) => tailrecM(f)(a)
          case Right(\/-(b)) => Right(b)
        }
    }

  /** [[scala.Either.LeftProjection]] is isomorphic to [[scala.Either]], when the type parameter `E` is partially applied. */
  def LeftProjectionEIso2[E]: LeftProjection[E, ?] <~> Either[E, ?] =
    LeftProjectionIso2.unlift1[E]

  /** [[scala.Either.LeftProjection]] is isomorphic to [[scala.Either]], when the type parameter `E` is partially applied. */
  def FirstLeftProjectionEIso2[E]: λ[α => LeftProjection[E, α] @@ First] <~> Either[E, ?] =
    FirstLeftProjectionIso2.unlift1[E]

  /** [[scala.Either.LeftProjection]] is isomorphic to [[scala.Either]], when the type parameter `E` is partially applied. */
  def LastLeftProjectionEIso2[E]: λ[α => LeftProjection[E, α] @@ Last] <~> Either[E, ?] =
    LastLeftProjectionIso2.unlift1[E]

  /** [[scala.Either.LeftProjection]] is isomorphic to [[scala.Either]] */
  val LeftProjectionIso2: LeftProjection <~~> Either =
    new IsoBifunctorTemplate[LeftProjection, Either] {
      def to[A, B](fa: LeftProjection[A, B]) = fa.e
      def from[A, B](ga: Either[A, B]) = ga.left
    }

  /** [[scala.Either.LeftProjection]] is isomorphic to [[scala.Either]] */
  val FirstLeftProjectionIso2: λ[(α, β) => LeftProjection[α, β] @@ First] <~~> Either =
    new IsoBifunctorTemplate[λ[(α, β) => LeftProjection[α, β] @@ First], Either] {
      def to[A, B](fa: LeftProjection[A, B] @@ First) = Tag.unwrap(fa).e
      def from[A, B](ga: Either[A, B]) = First(ga.left)
    }

  /** [[scala.Either.LeftProjection]] is isomorphic to [[scala.Either]] */
  val LastLeftProjectionIso2: λ[(α, β) => LeftProjection[α, β] @@ Last] <~~> Either =
    new IsoBifunctorTemplate[λ[(α, β) => LeftProjection[α, β] @@ Last], Either] {
      def to[A, B](fa: LeftProjection[A, B] @@ Last) = Tag.unwrap(fa).e
      def from[A, B](ga: Either[A, B]) = Last(ga.left)
    }

  /** [[scala.Either.RightProjection]] is isomorphic to [[scala.Either]], when the type parameter `A` is partially applied. */
  def RightProjectionAIso2[A]: RightProjection[?, A] <~> Either[?, A] =
    RightProjectionIso2.unlift2[A]

  /** [[scala.Either.RightProjection]] is isomorphic to [[scala.Either]], when the type parameter `A` is partially applied. */
  def FirstRightProjectionAIso2[A]: λ[α => RightProjection[α, A] @@ First] <~> Either[?, A] =
    FirstRightProjectionIso2.unlift2[A]

  /** [[scala.Either.RightProjection]] is isomorphic to [[scala.Either]], when the type parameter `A` is partially applied. */
  def LastRightProjectionAIso2[A]: λ[α => RightProjection[α, A] @@ Last] <~> Either[?, A] =
    LastRightProjectionIso2.unlift2[A]

  /** [[scala.Either.RightProjection]] is isomorphic to [[scala.Either]] */
  val RightProjectionIso2: RightProjection <~~> Either =
    new IsoBifunctorTemplate[RightProjection, Either] {
      def to[A, B](fa: RightProjection[A, B]) = fa.e
      def from[A, B](ga: Either[A, B]) = ga.right
    }

  /** [[scala.Either.RightProjection]] is isomorphic to [[scala.Either]] */
  val FirstRightProjectionIso2: λ[(α, β) => RightProjection[α, β] @@ First] <~~> Either =
    new IsoBifunctorTemplate[λ[(α, β) => RightProjection[α, β] @@ First], Either] {
      def to[A, B](fa: RightProjection[A, B] @@ First) = Tag.unwrap(fa).e
      def from[A, B](ga: Either[A, B]) = First(ga.right)
    }

  /** [[scala.Either.RightProjection]] is isomorphic to [[scala.Either]] */
  val LastRightProjectionIso2: λ[(α, β) => RightProjection[α, β] @@ Last] <~~> Either =
    new IsoBifunctorTemplate[λ[(α, β) => RightProjection[α, β] @@ Last], Either] {
      def to[A, B](fa: RightProjection[A, B] @@ Last) = Tag.unwrap(fa).e
      def from[A, B](ga: Either[A, B]) = Last(ga.right)
    }

  val eitherLeftInstance =
    new IsomorphismBifunctor[LeftProjection, Either] {
      def iso = LeftProjectionIso2
      implicit def G: Bifunctor[Either] = eitherInstance
    }

  val eitherFirstLeftInstance =
    new IsomorphismBifunctor[λ[(α, β) => LeftProjection[α, β] @@ First], Either] {
      def iso = FirstLeftProjectionIso2
      implicit def G: Bifunctor[Either] = eitherInstance
    }

  val eitherRightInstance =
    new IsomorphismBifunctor[RightProjection, Either] {
      def iso = RightProjectionIso2
      implicit def G: Bifunctor[Either] = eitherInstance
    }

  implicit def eitherRightLInstance[L] =
    new Monad[RightProjection[L, ?]] {
      def point[A](a: => A) = Right(a).right
      def bind[A, B](fa: RightProjection[L, A])(f: A => RightProjection[L, B]) = fa.e match {
        case Left(a)  => Left(a).right
        case Right(b) => f(b)
      }
    }

  implicit def eitherFirstRightLInstance[L]: Monad[λ[α => RightProjection[L, α] @@ First]] =
    Tags.First.subst1[Monad, RightProjection[L, ?]](Monad[RightProjection[L, ?]])

  implicit def eitherLastRightLInstance[L]: Monad[λ[α => RightProjection[L, α] @@ Last]] =
    Tags.Last.subst1[Monad, RightProjection[L, ?]](Monad[RightProjection[L, ?]])

  implicit def eitherLeftRInstance[R] =
    new Monad[LeftProjection[?, R]] {
      def point[A](a: => A) = Left(a).left
      def bind[A, B](fa: LeftProjection[A, R])(f: A => LeftProjection[B, R]) = fa.e match {
        case Left(a)  => f(a)
        case Right(b) => Right(b).left
      }
    }

  implicit def eitherFirstLeftRInstance[R]: Monad[λ[α => LeftProjection[α, R] @@ First]] =
    Tags.First.subst1[Monad, LeftProjection[?, R]](Monad[LeftProjection[?, R]])

  implicit def eitherLastLeftRInstance[R]: Monad[λ[α => LeftProjection[α, R] @@ Last]] =
    Tags.Last.subst1[Monad, LeftProjection[?, R]](Monad[LeftProjection[?, R]])

  implicit def eitherOrder[A, B](implicit OrderA: Order[A], OrderB: Order[B]): Order[Either[A, B]] =
    new EitherOrder[A, B] {
      implicit def A = OrderA
      implicit def B = OrderB
    }

  implicit def eitherLeftOrder[A, X](implicit OrderA: Order[A]): Order[LeftProjection[A, X]] =
    new EitherLeftOrder[A, X] {
      implicit def A = OrderA
    }

  implicit def eitherRightOrder[X, A](implicit OrderA: Order[A]): Order[RightProjection[X, A]] =
    new EitherRightOrder[X, A] {
      implicit def A = OrderA
    }

  implicit def eitherFirstLeftOrder[A, X](implicit OrderA: Order[A]): Order[LeftProjection[A, X] @@ First] = First.subst(Order[LeftProjection[A, X]])

  implicit def eitherFirstRightOrder[X, A](implicit OrderA: Order[A]): Order[RightProjection[X, A] @@ First] = First.subst(Order[RightProjection[X, A]])

  implicit def eitherLastLeftOrder[A, X](implicit OrderA: Order[A]): Order[LeftProjection[A, X] @@ Last] = Last.subst(Order[LeftProjection[A, X]])

  implicit def eitherLastRightOrder[X, A](implicit OrderA: Order[A]): Order[RightProjection[X, A] @@ Last] = Last.subst(Order[RightProjection[X, A]])

  implicit def eitherFirstLeftMonoid[A, X](implicit MonoidX: Monoid[X]): Monoid[LeftProjection[A, X] @@ First] =
    new EitherFirstLeftMonoid[A, X] {
      implicit def X = MonoidX
    }

  implicit def eitherFirstRightMonoid[X, A](implicit MonoidX: Monoid[X]): Monoid[RightProjection[X, A] @@ First] =
    new EitherFirstRightMonoid[X, A] {
      implicit def X = MonoidX
    }

  implicit def eitherLastLeftMonoid[A, X](implicit MonoidX: Monoid[X]): Monoid[LeftProjection[A, X] @@ Last] =
    new EitherLastLeftMonoid[A, X] {
      implicit def X = MonoidX
    }

  implicit def eitherLastRightMonoid[X, A](implicit MonoidX: Monoid[X]): Monoid[RightProjection[X, A] @@ Last] =
    new EitherLastRightMonoid[X, A] {
      implicit def X = MonoidX
    }

  implicit def eitherLeftMonoid[A, X](implicit MonoidA: Monoid[A], MonoidX: Monoid[X]): Monoid[LeftProjection[A, X]] =
    new EitherLeftMonoid[A, X] {
      implicit def A = MonoidA
      implicit def X = MonoidX
    }

  implicit def eitherRightMonoid[X, A](implicit MonoidX: Monoid[X], MonoidA: Monoid[A]): Monoid[RightProjection[X, A]] =
    new EitherRightMonoid[X, A] {
      implicit def X = MonoidX
      implicit def A = MonoidA
    }

  implicit def eitherAssociative: Associative[Either] = new Associative[Either] {
    override def reassociateLeft[A, B, C](f: Either[A, Either[B, C]]): Either[Either[A, B], C] =
      f.fold(
        a => Left(Left(a)),
        _.fold(
          b => Left(Right(b)),
          Right(_)
        )
      )

    override def reassociateRight[A, B, C](f: Either[Either[A, B], C]): Either[A, Either[B, C]] =
      f.fold(
        _.fold(
          Left(_),
          b => Right(Left(b))
        ),
        c => Right(Right(c))
      )

  }

  implicit def eitherShow[A,B](implicit SA: Show[A], SB: Show[B]) : Show[Either[A,B]] = new Show[Either[A,B]] {
    override def show(f: Either[A, B]): Cord = f match {
      case Left(a) => ("Left(" : Cord) ++ SA.show(a) :- ')'
      case Right(b) => ("Right(" : Cord) ++ SB.show(b) :- ')'
    }
  }
}

object either extends EitherInstances

private trait EitherRightEqual[X, A] extends Equal[RightProjection[X, A]] {
  implicit def A: Equal[A]

  final override def equal(a1: RightProjection[X, A], a2: RightProjection[X, A]) = (a1.toOption, a2.toOption) match {
    case (Some(x), Some(y)) => A.equal(x, y)
    case (None, None)       => true
    case _                  => false
  }
}

private trait EitherLeftEqual[A, X] extends Equal[LeftProjection[A, X]] {
  implicit def A: Equal[A]

  final override def equal(a1: LeftProjection[A, X], a2: LeftProjection[A, X]) = (a1.toOption, a2.toOption) match {
    case (Some(x), Some(y)) => A.equal(x, y)
    case (None, None)       => true
    case _                  => false
  }
}

private trait EitherEqual[A, B] extends Equal[Either[A, B]] {
  implicit def A: Equal[A]
  implicit def B: Equal[B]

  final override def equal(f1: Either[A, B], f2: Either[A, B]) = (f1, f2) match {
    case (Left(a1), Left(a2))                      => A.equal(a1, a2)
    case (Right(b1), Right(b2))                    => B.equal(b1, b2)
    case (Right(_), Left(_)) | (Left(_), Right(_)) => false
  }
  override val equalIsNatural: Boolean = A.equalIsNatural && B.equalIsNatural
}

private trait EitherFirstLeftSemigroup[A, X] extends Semigroup[LeftProjection[A, X] @@ First] {
  def append(f1: LeftProjection[A, X] @@ First, f2: => LeftProjection[A, X] @@ First) = if (Tag.unwrap(f1).e.isLeft) f1 else f2
}

private trait EitherFirstRightSemigroup[X, A] extends Semigroup[RightProjection[X, A] @@ First] {
  def append(f1: RightProjection[X, A] @@ First, f2: => RightProjection[X, A] @@ First) = if (Tag.unwrap(f1).e.isRight) f1 else f2
}

private trait EitherLastLeftSemigroup[A, X] extends Semigroup[LeftProjection[A, X] @@ Last] {
  def append(f1: LeftProjection[A, X] @@ Last, f2: => LeftProjection[A, X] @@ Last) = if (Tag.unwrap(f1).e.isLeft) f1 else f2
}

private trait EitherLastRightSemigroup[X, A] extends Semigroup[RightProjection[X, A] @@ Last] {
  def append(f1: RightProjection[X, A] @@ Last, f2: => RightProjection[X, A] @@ Last) = if (Tag.unwrap(f1).e.isRight) f1 else f2
}

private trait EitherLeftSemigroup[A, X] extends Semigroup[LeftProjection[A, X]] {
  implicit def A: Semigroup[A]
  implicit def X: Monoid[X]

  def append(f1: LeftProjection[A, X], f2: => LeftProjection[A, X]) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => Left(Semigroup[A].append(x, y)).left
    case (None, Some(_))    => f2
    case (Some(_), None)    => f1
    case (None, None)       => Right(Monoid[X].zero).left
  }
}

private trait EitherRightSemigroup[X, A] extends Semigroup[RightProjection[X, A]] {
  implicit def X: Monoid[X]
  implicit def A: Semigroup[A]

  def append(f1: RightProjection[X, A], f2: => RightProjection[X, A]) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => Right(Semigroup[A].append(x, y)).right
    case (None, Some(_))    => f2
    case (Some(_), None)    => f1
    case (None, None)       => Left(Monoid[X].zero).right
  }
}


private trait EitherFirstLeftMonoid[A, X] extends Monoid[LeftProjection[A, X] @@ First] with EitherFirstLeftSemigroup[A, X] {
  implicit def X: Monoid[X]

  def zero = First(Right(Monoid[X].zero).left)
}

private trait EitherLastLeftMonoid[A, X] extends Monoid[LeftProjection[A, X] @@ Last] with EitherLastLeftSemigroup[A, X] {
  implicit def X: Monoid[X]

  def zero = Last(Right(Monoid[X].zero).left)
}

private trait EitherLeftMonoid[A, X] extends Monoid[LeftProjection[A, X]] with EitherLeftSemigroup[A, X] {
  implicit def X: Monoid[X]

  def zero: LeftProjection[A, X] = Right(Monoid[X].zero).left
}

private trait EitherFirstRightMonoid[X, A] extends Monoid[RightProjection[X, A] @@ First] with EitherFirstRightSemigroup[X, A] {
  implicit def X: Monoid[X]

  def zero = First(Left(Monoid[X].zero).right)
}

private trait EitherLastRightMonoid[X, A] extends Monoid[RightProjection[X, A] @@ Last] with EitherLastRightSemigroup[X, A] {
  implicit def X: Monoid[X]

  def zero = Last(Left(Monoid[X].zero).right)
}

private trait EitherRightMonoid[X, A] extends Monoid[RightProjection[X, A]] with EitherRightSemigroup[X, A] {
  implicit def X: Monoid[X]

  def zero: RightProjection[X, A] = Left(Monoid[X].zero).right
}

private trait EitherOrder[A, B] extends Order[Either[A, B]] with EitherEqual[A, B]{
  implicit def A: Order[A]
  implicit def B: Order[B]

  import Ordering._

  def order(f1: Either[A, B], f2: Either[A, B]) = (f1, f2) match {
    case (Left(x), Left(y))   => A.order(x, y)
    case (Right(x), Right(y)) => B.order(x, y)
    case (Left(_), Right(_))  => LT
    case (Right(_), Left(_))  => GT
  }
}

private trait EitherLeftOrder[A, X] extends Order[LeftProjection[A, X]] with EitherLeftEqual[A, X]{
  implicit def A: Order[A]

  import Ordering._

  def order(f1: LeftProjection[A, X], f2: LeftProjection[A, X]) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => A.order(x, y)
    case (None, Some(_))    => LT
    case (Some(_), None)    => GT
    case (None, None)       => EQ
  }
}

private trait EitherRightOrder[X, A] extends Order[RightProjection[X, A]] with EitherRightEqual[X, A]{
  implicit def A: Order[A]

  import Ordering._

  def order(f1: RightProjection[X, A], f2: RightProjection[X, A]) = (f1.toOption, f2.toOption) match {
    case (Some(x), Some(y)) => A.order(x, y)
    case (None, Some(_))    => LT
    case (Some(_), None)    => GT
    case (None, None)       => EQ
  }
}

