package scalaz
package std

import scala.Either.{LeftProjection,  RightProjection}

trait Eithers {
  implicit def either[L] = new Monad[({type l[a]=Either[L, a]})#l] {
    def bind[A, B](fa: Either[L, A])(f: (A) => Either[L, B]): Either[L, B] = fa.right.flatMap(f)

    def pure[A](a: => A): Either[L, A] = Right(a)
  }

  implicit def eitherSemigroup[A, B] = new EitherSemigroup[A, B] {}

  implicit def eitherMonoid[A: Monoid, B] = new EitherSemigroup[A, B] {}

  implicit def eitherLeftSemigroup[A, B] = new EitherLeftSemigroup[A, B] {}

  implicit def eitherRightSemigroup[A, B] = new EitherRightSemigroup[A, B] {}

  implicit def eitherLeftMonoid[A, B: Monoid] = new Monoid[LeftProjection[A, B]] with EitherLeftSemigroup[A, B] {
    def zero: LeftProjection[A, B] = Right(Monoid[B].zero).left
  }

  implicit def eitherRightMonoid[A: Monoid, B] = new Monoid[RightProjection[A, B]] with EitherRightSemigroup[A, B] {
    def zero: RightProjection[A, B] = Left(Monoid[A].zero).right
  }
}

object Either extends Eithers

sealed trait EitherSemigroup[A, B] extends Semigroup[Either[A, B]] {
  // TODO does this instance make sense?
  def append(f1: Either[A, B], f2: => Either[A, B]): Either[A, B] = if (f1.isLeft) f1 else f2
}

sealed trait EitherLeftSemigroup[A, B] extends Semigroup[LeftProjection[A, B]] {
  def append(f1: LeftProjection[A, B], f2: => LeftProjection[A, B]) = if (f1.e.isLeft) f1 else f2
}

sealed trait EitherRightSemigroup[A, B] extends Semigroup[RightProjection[A, B]] {
  def append(f1: RightProjection[A, B], f2: => RightProjection[A, B]) = if (f1.e.isLeft) f1 else f2
}