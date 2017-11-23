package scalaz
package data

import typeclass._

sealed abstract class These[L, R] {
  import These.{This, That, Both}

  /**
    * The canonical catamorphism from `These` values.
    */
  final def fold[C](fl: L => C)(fr: R => C)(fboth: (L, R) => C): C = this match {
    case This(left)        => fl(left)
    case That(right)       => fr(right)
    case Both(left, right) => fboth(left, right)
  }

  final def thisSide: Option[L] = this match {
    case This(left)    => Some(left)
    case That(_)       => None
    case Both(left, _) => Some(left)
  }

  final def thatSide: Option[R] = this match {
    case This(_)        => None
    case That(right)    => Some(right)
    case Both(_, right) => Some(right)
  }

  final def both: Option[(L, R)] = this match {
    case Both(left, right) => Some((left, right))
    case _                 => None
  }

  final def swap: These[R, L] = this match {
    case This(left)        => That(left)
    case That(right)       => This(right)
    case Both(left, right) => Both(right, left)
  }

  /* Bifunctor */
  final def bimap[C, D](fl: L => C)(fr: R => D): These[C, D] = this match {
    case This(left)        => This(fl(left))
    case That(right)       => That(fr(right))
    case Both(left, right) => Both(fl(left), fr(right))
  }
  final def lmap[C](fl: L => C): These[C, R] = this match {
    case This(left)        => This(fl(left))
    case That(right)       => That(right)
    case Both(left, right) => Both(fl(left), right)
  }
  final def rmap[D](fr: R => D): These[L, D] = this match {
    case This(left)        => This(left)
    case That(right)       => That(fr(right))
    case Both(left, right) => Both(left, fr(right))
  }

  /* Monad (on the right) */
  final def flatMap[D](f: R => These[L, D])(implicit L: Semigroup[L]): These[L, D] = this match {
    case This(left)        => This(left)
    case That(right)       => f(right)
    case Both(left, right) => f(right) match {
      case This(left1)         => This(L.append(left, left1))
      case That(right1)        => Both(left, right1)
      case Both(left1, right1) => Both(L.append(left, left1), right1)
    }
  }

  /* Foldable (on the right) */
  final def foldMap[D](f: R => D)(implicit D: Monoid[D]): D = this match {
    case This(_)        => D.empty
    case That(right)    => f(right)
    case Both(_, right) => f(right)
  }

  final def foldRight[B](z: => B)(f: (R, => B) => B): B =
    foldLeft(z)((b, r) => f(r, b))

  final def foldLeft[B](z: B)(f: (B, R) => B): B = this match {
    case This(_)        => z
    case That(right)    => f(z, right)
    case Both(_, right) => f(z, right)
  }

  /* Traversable (on the right) */
  final def traverse[F[_], B](f: R => F[B])(implicit F: Applicative[F]): F[These[L, B]] = this match {
    case This(left)        => F.pure(This(left))
    case That(right)       => F.apply.functor.map(f(right))(That(_))
    case Both(left, right) => F.apply.functor.map(f(right))(Both(left, _))
  }

  /* Semigroup */
  final def append(other: These[L, R])(implicit L: Semigroup[L], R: Semigroup[R]): These[L, R] = (this, other) match {
    case (This(l1),     This(l2)    ) => This(L.append(l1, l2))
    case (This(l1),     That(r2)    ) => Both(l1, r2)
    case (This(l1),     Both(l2, r2)) => Both(L.append(l1, l2), r2)
    case (That(r1),     This(l2)    ) => Both(l2, r1)
    case (That(r1),     That(r2)    ) => That(R.append(r1, r2))
    case (That(r1),     Both(l2, r2)) => Both(l2, R.append(r1, r2))
    case (Both(l1, r1), This(l2)    ) => Both(L.append(l1, l2), r1)
    case (Both(l1, r1), That(r2)    ) => Both(l1, R.append(r1, r2))
    case (Both(l1, r1), Both(l2, r2)) => Both(L.append(l1, l2), R.append(r1, r2))
  }

}

object These extends TheseInstances {
  type \&/[A, B] = These[A, B]

  /* ADT cases */
  final case class This[L, R](thisValue: L)               extends These[L, R]
  final case class That[L, R](thatValue: R)               extends These[L, R]
  final case class Both[L, R](thisValue: L, thatValue: R) extends These[L, R]

  /* "smart" constructors */
  object This {
    @inline def apply[L, R](thisValue: L): These[L, R] = new This[L, R](thisValue)
  }
  object That {
    @inline def apply[L, R](thatValue: R): These[L, R] = new That[L, R](thatValue)
  }
  object Both {
    @inline def apply[L, R](thisValue: L, thatValue: R): These[L, R] = new Both[L, R](thisValue, thatValue)
  }
}
