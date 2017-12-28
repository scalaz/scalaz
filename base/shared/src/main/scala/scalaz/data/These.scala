package scalaz
package data

sealed abstract class These[L, R] {
  /**
    * The canonical catamorphism from `These` values.
    */
  final def fold[C](fl: L => C)(fr: R => C)(fboth: (L, R) => C): C = this match {
    case This(left)        => fl(left)
    case That(right)       => fr(right)
    case Both(left, right) => fboth(left, right)
  }

  final def thisSide: Maybe[L] = this match {
    case This(left)    => Maybe.just(left)
    case That(_)       => Maybe.empty
    case Both(left, _) => Maybe.just(left)
  }

  final def thatSide: Maybe[R] = this match {
    case This(_)        => Maybe.empty
    case That(right)    => Maybe.just(right)
    case Both(_, right) => Maybe.just(right)
  }

  final def both: Maybe[(L, R)] = this match {
    case Both(left, right) => Maybe.just((left, right))
    case _                 => Maybe.empty
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
    case that @ That(_)    => that.pmap[C]
    case Both(left, right) => Both(fl(left), right)
  }
  final def rmap[D](fr: R => D): These[L, D] = this match {
    case thiz @ This(_)    => thiz.pmap[D]
    case That(right)       => That(fr(right))
    case Both(left, right) => Both(left, fr(right))
  }

  /* Applicative (on the right) */
  final def ap[D](f: These[L, R => D])(implicit L: Semigroup[L]): These[L, D] = this match {
    case thiz @ This(_)    => thiz.pmap[D]
    case That(right)       => f match {
      case thiz1 @ This(_)     => thiz1.pmap[D]
      case That(right1)        => That(right1(right))
      case Both(left1, right1) => Both(left1, right1(right))
    }
    case Both(left, right) => f match {
      case This(left1)         => This(L.append(left, left1))
      case That(right1)        => Both(left, right1(right))
      case Both(left1, right1) => Both(L.append(left, left1), right1(right))
    }
  }

  /* Monad (on the right) */
  final def flatMap[D](f: R => These[L, D])(implicit L: Semigroup[L]): These[L, D] = this match {
    case thiz @ This(_)    => thiz.pmap[D]
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
    case thiz @ This(_)    => F.pure(thiz.pmap[B])
    case That(right)       => F.apply.functor.map(f(right))(That(_))
    case Both(left, right) => F.apply.functor.map(f(right))(Both(left, _))
  }

  /* Semigroup */
  final def append(other: These[L, R])(implicit L: Semigroup[L], R: Semigroup[R]): These[L, R] = other match {
    case This(left)        => lappend(left)
    case That(right)       => rappend(right)
    case Both(left, right) => lappend(left).rappend(right)
  }

  final def lappend(other: L)(implicit L: Semigroup[L]): These[L, R] = this match {
    case This(left)        => This(L.append(left, other))
    case That(right)       => Both(other, right)
    case Both(left, right) => Both(L.append(left, other), right)
  }

  final def rappend(other: R)(implicit R: Semigroup[R]): These[L, R] = this match {
    case This(left)        => Both(left, other)
    case That(right)       => That(R.append(right, other))
    case Both(left, right) => Both(left, R.append(right, other))
  }

}

final case class This[L, R](thisValue: L) extends These[L, R] {
  @inline private[data] final def pmap[RR]: This[L, RR] = this.asInstanceOf[This[L, RR]]
}

final case class That[L, R](thatValue: R)               extends These[L, R] {
  @inline private[data] final def pmap[LL]: That[LL, R] = this.asInstanceOf[That[LL, R]]
}

final case class Both[L, R](thisValue: L, thatValue: R) extends These[L, R]

object This {
  @inline final def apply[L, R](thisValue: L): These[L, R] = new This[L, R](thisValue)
}

object That {
  @inline final def apply[L, R](thatValue: R): These[L, R] = new That[L, R](thatValue)
}

object Both {
  @inline final def apply[L, R](thisValue: L, thatValue: R): These[L, R] = new Both[L, R](thisValue, thatValue)
}
