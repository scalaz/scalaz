package scalaz

/**
 * Represents disjunction. Isomorphic to `scala.Either`. Does not have left/right projections, instead right-bias and use `swap` or `swapped`.
 */
sealed trait \/[+A, +B] {
  sealed trait SwitchingDisjunction[X] {
    def r: X
    def <<?:(left: => X): X =
      \/.this match {
        case -\/(_) => left
        case \/-(_) => r
      }
  }

  /** If this disjunction is right, return the given X value, otherwise, return the X value given to the return value. */
  def :?>>[X](right: => X): SwitchingDisjunction[X] =
    new SwitchingDisjunction[X] {
      def r = right
    }

  /** Return `true` if this disjunction is left. */
  def isLeft: Boolean =
    this match {
      case -\/(_) => true
      case \/-(_) => false
    }

  /** Return `true` if this disjunction is right. */
  def isRight: Boolean =
    this match {
      case -\/(_) => false
      case \/-(_) => true
    }

  /** Catamorphism. Run the first given function if left, otherwise, the second given function. */
  def fold[X](l: A => X, r: B => X): X =
    this match {
      case -\/(a) => l(a)
      case \/-(b) => r(b)
    }

  /** Spin in tail-position on the right value of this disjunction. */
  def loopr[AA >: A, BB >: B, X](left: AA => X, right: BB => X \/ (AA \/ BB)): X =
    \/.loopRight(this, left, right)

  /** Spin in tail-position on the left value of this disjunction. */
  def loopl[AA >: A, BB >: B, X](left: AA => X \/ (AA \/ BB), right: BB => X): X =
    \/.loopLeft(this, left, right)

  /** Flip the left/right values in this disjunction. Alias for `unary_~` */
  def swap: (B \/ A) =
    this match {
      case -\/(a) => \/-(a)
      case \/-(b) => -\/(b)
    }

  /** Flip the left/right values in this disjunction. Alias for `swap` */
  def unary_~ : (B \/ A) =
    swap

  /** Run the given function on this swapped value. Alias for `~` */
  def swapped[AA, BB](k: (B \/ A) => (BB \/ AA)): (AA \/ BB) =
    k(swap).swap

  /** Run the given function on this swapped value. Alias for `swapped` */
  def ~[AA, BB](k: (B \/ A) => (BB \/ AA)): (AA \/ BB) =
    swapped(k)

  /** Binary functor map on this disjunction. */
  def bimap[C, D](f: A => C, g: B => D): (C \/ D) =
    this match {
      case -\/(a) => -\/(f(a))
      case \/-(b) => \/-(g(b))
    }

  /** Run the given function on the left value. */
  def leftMap[C](f: A => C): (C \/ B) =
    bimap(f, identity)

  /** Binary functor traverse on this disjunction. */
  def bitraverse[F[_]: Functor, C, D](f: A => F[C], g: B => F[D]): F[C \/ D] =
    this match {
      case -\/(a) => Functor[F].map(f(a))(-\/(_))
      case \/-(b) => Functor[F].map(g(b))(\/-(_))
    }

  /** Map on the right of this disjunction. */
  def map[D](g: B => D): (A \/ D) =
    bimap(identity, g)

  /** Traverse on the right of this disjunction. */
  def traverse[F[_]: Applicative, AA >: A, D](g: B => F[D]): F[AA \/ D] =
    this match {
      case a @ -\/(_) => Applicative[F].point(a)
      case \/-(b) => Functor[F].map(g(b))(\/-(_))
    }

  /** Run the side-effect on the right of this disjunction. */
  def foreach(g: B => Unit): Unit =
    bimap(_ => (), g)

  /** Apply a function in the environment of the right of this disjunction. */
  def ap[AA >: A, C](f: => AA \/ (B => C)): (AA \/ C) =
    f flatMap (ff => map(ff(_)))

  /** Bind through the right of this disjunction. */
  def flatMap[AA >: A, D](g: B => (AA \/ D)): (AA \/ D) =
    this match {
      case a @ -\/(_) => a
      case \/-(b) => g(b)
    }

  /** Fold on the right of this disjunction. */
  def foldRight[Z](z: => Z)(f: (B, => Z) => Z): Z =
    this match {
      case -\/(_) => z
      case \/-(a) => f(a, z)
    }

  /** Filter on the right of this disjunction. */
  def filter[AA >: A](p: B => Boolean)(implicit M: Monoid[AA]): (AA \/ B) =
    this match {
      case -\/(_) => this
      case \/-(b) => if(p(b)) this else -\/(M.zero)
    }

  /** Return `true` if this disjunction is a right value satisfying the given predicate. */
  def exists[BB >: B](p: BB => Boolean): Boolean =
    this match {
      case -\/(_) => false
      case \/-(b) => p(b)
    }

  /** Return `true` if this disjunction is a left value or the right value satisfies the given predicate. */
  def forall[BB >: B](p: BB => Boolean): Boolean =
    this match {
      case -\/(_) => true
      case \/-(b) => p(b)
    }

  /** Return an empty list or list with one element on the right of this disjunction. */
  def toList: List[B] =
    this match {
      case -\/(_) => Nil
      case \/-(b) => List(b)
    }

  /** Return an empty stream or stream with one element on the right of this disjunction. */
  def toStream: Stream[B] =
    this match {
      case -\/(_) => Stream()
      case \/-(b) => Stream(b)
    }

  /** Return an empty option or option with one element on the right of this disjunction. Useful to sweep errors under the carpet. */
  def toOption: Option[B] =
    this match {
      case -\/(_) => None
      case \/-(b) => Some(b)
    }

  /** Convert to a core `scala.Either` at your own peril. */
  def toEither: Either[A, B] =
    this match {
      case -\/(a) => Left(a)
      case \/-(b) => Right(b)
    }

  /** Return the right value of this disjunction or the given default if left. Alias for `|` */
  def getOrElse[BB >: B](x: => BB): BB =
    toOption getOrElse x

  /** Return the right value of this disjunction or the given default if left. Alias for `getOrElse` */
  def |[BB >: B](x: => BB): BB =
    getOrElse(x)

  /** Return the right value of this disjunction or run the given function on the left. */
  def valueOr[BB >: B](x: A => BB): BB =
    this match {
      case -\/(a) => x(a)
      case \/-(b) => b
    }

  /** Return this if it is a right, otherwise, return the given value. Alias for `|||` */
  def orElse[AA >: A, BB >: B](x: => AA \/ BB): AA \/ BB =
    this match {
      case -\/(_) => x
      case \/-(_) => this
    }

  /** Return this if it is a right, otherwise, return the given value. Alias for `orElse` */
  def |||[AA >: A, BB >: B](x: => AA \/ BB): AA \/ BB =
    orElse(x)

  /**
   * Sums up values inside disjunction, if both are left or right. Returns first left otherwise.
   * {{{
   * \/-(v1) +++ \/-(v2) → \/-(v1 + v2)
   * \/-(v1) +++ -\/(v2) → -\/(v2)
   * -\/(v1) +++ \/-(v2) → -\/(v1)
   * -\/(v1) +++ -\/(v2) → -\/(v1 + v2)
   * }}}
   */
  def +++[AA >: A, BB >: B](x: => AA \/ BB)(implicit M1: Semigroup[BB], M2: Semigroup[AA]): AA \/ BB =
    this match {
      case -\/(a1) => x match {
        case -\/(a2) => -\/(M2.append(a1, a2))
        case \/-(_) => this
      }
      case \/-(b1) => x match {
        case -\/(_) => x
        case \/-(b2) => \/-(M1.append(b1, b2))
      }
    }

  /** Ensures that the right value of this disjunction satisfies the given predicate, or returns left with the given value. */
  def ensure[AA >: A](onLeft: => AA)(f: B => Boolean): (AA \/ B) = this match {
    case \/-(b) => if (f(b)) this else -\/(onLeft)
    case -\/(_) => this
  }

  /** Compare two disjunction values for equality. */
  def ===[AA >: A, BB >: B](x: AA \/ BB)(implicit EA: Equal[AA], EB: Equal[BB]): Boolean =
    this match {
      case -\/(a1) => x match {
        case -\/(a2) => Equal[AA].equal(a1, a2)
        case \/-(_) => false
      }
      case \/-(b1) => x match {
        case \/-(b2) => Equal[BB].equal(b1, b2)
        case -\/(_) => false
      }
    }

  /** Compare two disjunction values for ordering. */
  def compare[AA >: A, BB >: B](x: AA \/ BB)(implicit EA: Order[AA], EB: Order[BB]): Ordering =
    this match {
      case -\/(a1) => x match {
        case -\/(a2) => Order[AA].apply(a1, a2)
        case \/-(_) => Ordering.LT
      }
      case \/-(b1) => x match {
        case \/-(b2) => Order[BB].apply(b1, b2)
        case -\/(_) => Ordering.GT
      }
    }

  /** Show for a disjunction value. */
  def show[AA >: A, BB >: B](implicit SA: Show[AA], SB: Show[BB]): Cord =
    this match {
      case -\/(a) => ("-\\/(": Cord) ++ SA.show(a) :- ')'
      case \/-(b) => ("\\/-(": Cord) ++ SB.show(b) :- ')'
    }

  /** Convert to a validation. */
  def validation: Validation[A, B] =
    this match {
      case -\/(a) => Failure(a)
      case \/-(b) => Success(b)
    }

  /** Run a validation function and back to disjunction again. Alias for `@\?/` */
  def validationed[AA, BB](k: Validation[A, B] => Validation[AA, BB]): AA \/ BB =
    k(validation).disjunction

  /** Run a validation function and back to disjunction again. Alias for `validationed` */
  def @\?/[AA, BB](k: Validation[A, B] => Validation[AA, BB]): AA \/ BB =
    validationed(k)


}
case class -\/[+A](a: A) extends (A \/ Nothing)
case class \/-[+B](b: B) extends (Nothing \/ B)

object \/ extends DisjunctionInstances with DisjunctionFunctions {

  /** Spin in tail-position on the right value of the given disjunction. */
  @annotation.tailrec
  final def loopRight[A, B, X](d: A \/ B, left: A => X, right: B => X \/ (A \/ B)): X =
    d match {
      case -\/(a) => left(a)
      case \/-(b) => right(b) match {
        case -\/(x) => x
        case \/-(q) => loopRight(q, left, right)
      }
    }

  /** Spin in tail-position on the left value of the given disjunction. */
  @annotation.tailrec
  final def loopLeft[A, B, X](d: A \/ B, left: A => X \/ (A \/ B), right: B => X): X =
    d match {
      case -\/(a) => left(a) match {
        case -\/(x) => x
        case \/-(q) => loopLeft(q, left, right)
      }
      case \/-(b) => right(b)
    }

}

trait DisjunctionInstances extends DisjunctionInstances0 {
  /** Turns out that Either is just a glorified tuple; who knew? */
  type GlorifiedTuple[+A, +B] =
  A \/ B
}

trait DisjunctionInstances0 extends DisjunctionInstances1 {
  implicit def DisjunctionOrder[A: Order, B: Order]: Order[A \/ B] =
    new Order[A \/ B] {
      def order(a1: A \/ B, a2: A \/ B) =
        a1 compare a2
    }

  implicit def DisjunctionMonoid[A: Semigroup, B: Monoid]: Monoid[A \/ B] =
    new Monoid[A \/ B] {
      def append(a1: A \/ B, a2: => A \/ B) =
        a1 +++ a2
      def zero =
        \/-(Monoid[B].zero)
    }
}

trait DisjunctionInstances1 extends DisjunctionInstances2 {
  implicit def DisjunctionEqual[A: Equal, B: Equal]: Equal[A \/ B] =
    new Equal[A \/ B] {
      def equal(a1: A \/ B, a2: A \/ B) =
        a1 === a2
    }

  implicit def DisjunctionShow[A: Show, B: Show]: Show[A \/ B] =
    Show.show(_.show)

  implicit def DisjunctionSemigroup[A: Semigroup, B: Semigroup]: Semigroup[A \/ B] =
    new Semigroup[A \/ B] {
      def append(a1: A \/ B, a2: => A \/ B) =
        a1 +++ a2
    }
}

trait DisjunctionInstances2 extends DisjunctionInstances3 {
  implicit def DisjunctionInstances2[L]: Traverse[({type l[a] = L \/ a})#l] with Monad[({type l[a] = L \/ a})#l] with Cozip[({type l[a] = L \/ a})#l] with Plus[({type l[a] = L \/ a})#l] with Optional[({type l[a] = L \/ a})#l] = new Traverse[({type l[a] = L \/ a})#l] with Monad[({type l[a] = L \/ a})#l] with Cozip[({type l[a] = L \/ a})#l] with Plus[({type l[a] = L \/ a})#l] with Optional[({type l[a] = L \/ a})#l] {
    def bind[A, B](fa: L \/ A)(f: A => L \/ B) =
      fa flatMap f

    def point[A](a: => A) =
      \/-(a)

    def traverseImpl[G[_] : Applicative, A, B](fa: L \/ A)(f: A => G[B]) =
      fa.traverse(f)

    override def foldRight[A, B](fa: L \/ A, z: => B)(f: (A, => B) => B) =
      fa.foldRight(z)(f)

    def cozip[A, B](x: L \/ (A \/ B)) =
      x match {
        case l @ -\/(_) => -\/(l)
        case \/-(e) => e match {
          case -\/(a) => -\/(\/-(a))
          case b @ \/-(_) => \/-(b)
        }
      }

    def plus[A](a: L \/ A, b: => L \/ A) =
      a orElse b

    def pextract[B, A](fa: L \/ A): (L \/ B) \/ A = fa match {
      case l@ -\/(_) => -\/(l)
      case r@ \/-(_) => r
    }
  }

}

trait DisjunctionInstances3 {
  implicit def DisjunctionInstances3 : Bitraverse[\/] = new Bitraverse[\/] {
    override def bimap[A, B, C, D](fab: A \/ B)
                                  (f: A => C, g: B => D) = fab bimap (f, g)

    def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: A \/ B)
                                                  (f: A => G[C], g: B => G[D]) =
      fab.bitraverse(f, g)
  }
}

trait DisjunctionFunctions {
  /** Construct a left disjunction value. */
  def left[A, B]: A => A \/ B =
    -\/(_)

  /** Construct a right disjunction value. */
  def right[A, B]: B => A \/ B =
    \/-(_)

  /** Construct a disjunction value from a standard `scala.Either`. */
  def fromEither[A, B](e: Either[A, B]): A \/ B =
    e fold (left, right)

  /** Evaluate the given value, which might throw an exception. */
  def fromTryCatch[T](a: => T): Throwable \/ T = try {
    right(a)
  } catch {
    case e: Throwable => left(e)
  }
}
