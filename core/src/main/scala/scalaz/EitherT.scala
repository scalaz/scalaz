package scalaz

import scala.util.control.NonFatal
import scala.reflect.ClassTag
import Liskov.<~<

/**
 * Represents a computation of type `F[A \/ B]`.
 *
 * Example:
 * {{{
 * val x: Option[String \/ Int] = Some(\/-(1))
 * EitherT(x).map(1+).run // Some(\/-(2))
 * }}}
 * */
final case class EitherT[A, F[_], B](run: F[A \/ B]) {
  import OptionT._

  final class Switching_\/[X](r: => X) {
    def <<?:(left: X)(implicit F: Functor[F]): F[X] =
      foldConst(left, r)
  }

  /** If this disjunction is right, return the given X value, otherwise, return the X value given to the return value. */
  @deprecated("Due to SI-1980, <<?: will always evaluate its left argument; use foldConst instead",
              since = "7.3.0")
  def :?>>[X](right: => X): Switching_\/[X] =
    new Switching_\/(right)

  def fold[X](l: A => X, r: B => X)(implicit F: Functor[F]): F[X] =
    F.map(run)(_.fold(l, r))

  def foldM[X](l: A => F[X], r: B => F[X])(implicit F: Bind[F]): F[X] =
    F.join(fold(l, r))

  def foldConst[X](l: => X, r: => X)(implicit F: Functor[F]): F[X] =
    F.map(run)(_.foldConst(l, r))

  /** Return `true` if this disjunction is left. */
  def isLeft(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isLeft)

  /** Return `true` if this disjunction is right. */
  def isRight(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isRight)

  /** Flip the left/right values in this disjunction. Alias for `swap` */
  def swap(implicit F: Functor[F]): EitherT[B, F, A] =
    EitherT(F.map(run)(_.swap))

  /** Flip the left/right values in this disjunction. Alias for `unary_~` */
  def unary_~(implicit F: Functor[F]): EitherT[B, F, A] =
    swap

  /** Run the given function on this swapped value. Alias for `~` */
  def swapped[AA, BB](k: (B \/ A) => (BB \/ AA))(implicit F: Functor[F]): EitherT[AA, F, BB] =
    EitherT(F.map(run)(_ swapped k))

  /** Run the given function on this swapped value. Alias for `swapped` */
  def ~[AA, BB](k: (B \/ A) => (BB \/ AA))(implicit F: Functor[F]): EitherT[AA, F, BB] =
    swapped(k)

  /** Binary functor map on this disjunction. */
  def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): EitherT[C, F, D] =
    EitherT(F.map(run)(_.bimap(f, g)))

  /** Run the given function on the left value. */
  def leftMap[C](f: A => C)(implicit F: Functor[F]): EitherT[C, F, B] =
    bimap(f, identity)

  /** Binary functor traverse on this disjunction. */
  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit F: Traverse[F], G: Applicative[G]): G[EitherT[C, F, D]] =
    Applicative[G].map(F.traverse(run)(Bitraverse[\/].bitraverseF(f, g)))(EitherT(_: F[C \/ D]))

  /** Map on the right of this disjunction. */
  def map[C](f: B => C)(implicit F: Functor[F]): EitherT[A, F, C] =
    EitherT(F.map(run)(_.map(f)))

  /** Map on the right of this disjunction. */
  def mapF[C](f: B => F[C])(implicit M: Monad[F]): EitherT[A, F, C] =
    flatMapF {
      f andThen (mb => M.map(mb)(b => \/-(b)))
    }

  def mapT[G[_], C, D](f: F[A \/ B] => G[C \/ D]): EitherT[C, G, D] =
    EitherT(f(run))

  /** Traverse on the right of this disjunction. */
  def traverse[G[_], C](f: B => G[C])(implicit F: Traverse[F], G: Applicative[G]): G[EitherT[A, F, C]] =
    G.map(F.traverse(run)(o => Traverse[A \/ ?].traverse(o)(f)))(EitherT(_))

  /** Apply a function in the environment of the right of this
    * disjunction.  Because it runs my `F` even when `f`'s `\/` fails,
    * it is not consistent with `ap`.
    */
  def app[C](f: => EitherT[A, F, B => C])(implicit F: Apply[F]): EitherT[A, F, C] =
    EitherT(F.apply2(f.run, run)((a, b) => b ap a))

  /** Bind through the right of this disjunction. */
  def flatMap[C](f: B => EitherT[A, F, C])(implicit F: Monad[F]): EitherT[A, F, C] =
    EitherT(F.bind(run)(_.fold(a => F.point(-\/(a): (A \/ C)), b => f(b).run)))

  /** Bind the inner monad through the right of this disjunction. */
  def flatMapF[C](f: B => F[A \/ C])(implicit F: Monad[F]): EitherT[A, F, C] =
    EitherT(F.bind(run)(_.fold(a => F.point(-\/(a): (A \/ C)), f)))

  /** Fold on the right of this disjunction. */
  def foldRight[Z](z: => Z)(f: (B, => Z) => Z)(implicit F: Foldable[F]): Z =
    F.foldRight[A \/ B, Z](run, z)((a, b) => a.foldRight(b)(f))

  /** Filter on the right of this disjunction. */
  def filter(p: B => Boolean)(implicit M: Monoid[A], F: Monad[F]): EitherT[A, F, B] =
    MonadPlus[EitherT[A, F, ?]].filter(this)(p)

  /** Alias for `filter`.
   */
  def withFilter(p: B => Boolean)(implicit M: Monoid[A], F: Monad[F]): EitherT[A, F, B] =
    filter(p)(M, F)

  /** Return `true` if this disjunction is a right value satisfying the given predicate. */
  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ exists f)

  /** Return `true` if this disjunction is a left value or the right value satisfies the given predicate. */
  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ forall f)

  /** Return an empty list or list with one element on the right of this disjunction. */
  def toList(implicit F: Functor[F]): F[List[B]] =
    F.map(run)(_.fold(_ => Nil, _ :: Nil))

  /** Return a `this` on the left-side or a `that` on the right-side of this disjunction  */
  def toThese(implicit F: Functor[F]): TheseT[F, A, B] = TheseT(F.map(run)(_.toThese))

  /** Return an empty stream or stream with one element on the right of this disjunction. */
  def toStream(implicit F: Functor[F]): F[Stream[B]] =
    F.map(run)((_: (A \/ B)).fold(_ => Stream(), Stream(_)))

  /** Return an empty option or option with one element on the right of this disjunction. Useful to sweep errors under the carpet. */
  def toOption(implicit F: Functor[F]): OptionT[F, B] =
    optionT[F](F.map(run)((_: (A \/ B)).toOption))

  /** Return an empty option or option with one element on the right of this disjunction. Useful to sweep errors under the carpet. */
  def toMaybe(implicit F: Functor[F]): MaybeT[F, B] =
    MaybeT(F.map(run)((_: (A \/ B)).toMaybe))

  /** Convert to a core `scala.Either` at your own peril. */
  def toEither(implicit F: Functor[F]): F[Either[A, B]] =
    F.map(run)(_.toEither)

  /** Return the right value of this disjunction or the given default if left. Alias for `|` */
  def getOrElse(default: => B)(implicit F: Functor[F]): F[B] =
    F.map(run)(_ getOrElse default)

  /** Return the right value of this disjunction or the given default if left. Alias for `getOrElse` */
  def |(default: => B)(implicit F: Functor[F]): F[B] =
    getOrElse(default)

  /** Return the right value of this disjunction or run the given function on the left. */
  def valueOr(x: A => B)(implicit F: Functor[F]): F[B] =
    F.map(run)(_ valueOr x)

  /** Return this if it is a right, otherwise, return the given value. Alias for `|||` */
  def orElse(x: => EitherT[A, F, B])(implicit F: Monad[F]): EitherT[A, F, B] = {
    val g = run
    EitherT(F.bind(g) {
      case    -\/(_)  => x.run
      case r@(\/-(_)) => F.point(r)
    })
  }

  /** Return this if it is a right, otherwise, return the given value. Alias for `orElse` */
  def |||(x: => EitherT[A, F, B])(implicit F: Monad[F]): EitherT[A, F, B] =
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
  def +++(x: => EitherT[A, F, B])(implicit M1: Semigroup[B], M2: Semigroup[A], F: Apply[F]): EitherT[A, F, B] =
    EitherT(F.apply2(run, x.run)(_ +++ _))

  /** Ensures that the right value of this disjunction satisfies the given predicate, or returns left with the given value. */
  def ensure(onLeft: => A)(f: B => Boolean)(implicit F: Functor[F]): EitherT[A, F, B] =
    EitherT(F.map(run)(_.ensure(onLeft)(f)))

  /** Compare two disjunction values for equality. */
  def ===(x: EitherT[A, F, B])(implicit EA: Equal[A], EB: Equal[B], F: Apply[F]): F[Boolean] =
    F.apply2(run, x.run)(_ === _)

  /** Compare two disjunction values for ordering. */
  def compare(x: EitherT[A, F, B])(implicit EA: Order[A], EB: Order[B], F: Apply[F]): F[Ordering] =
    F.apply2(run, x.run)(_ compare _)

  /** Show for a disjunction value. */
  def show(implicit SA: Show[A], SB: Show[B], F: Functor[F]): F[Cord] =
    F.map(run)(_.show[A, B])

  /** Cozip this disjunction on its functor. */
  def cozip(implicit Z: Cozip[F]): (F[A] \/ F[B]) =
    Z.cozip(run)

  /** Convert to a validation. */
  def toValidation(implicit F: Functor[F]): F[Validation[A, B]] =
    F.map(run)(_.toValidation)

  @deprecated("Use `toValidation`", "7.3.0")
  def validation(implicit F: Functor[F]): F[Validation[A, B]] = toValidation

  /** Run a validation function and back to disjunction again. */
  def validationed[AA, BB](k: Validation[A, B] => Validation[AA, BB])(implicit F: Functor[F]): EitherT[AA, F, BB] =
    EitherT(F.map(run)(_ validationed k))

  /** Return the value from whichever side of the disjunction is defined, given a commonly assignable type. */
  def merge[AA >: A](implicit F: Functor[F], ev: B <~< AA): F[AA] = {
    F.map(run) {
      case -\/(a) => a
      case \/-(b) => ev(b)
    }
  }
}

object EitherT extends EitherTInstances {
  def eitherT[A, F[_], B](a: F[A \/ B]): EitherT[A, F, B] = apply(a)
  def either[A, F[_]: Applicative, B](d: A \/ B): EitherT[A, F, B] = apply(Applicative[F].point(d))
  def leftT[A, F[_]: Functor, B](fa: F[A]): EitherT[A, F, B] = apply(Functor[F].map(fa)(-\/(_)))
  def rightT[A, F[_]: Functor, B](fb: F[B]): EitherT[A, F, B] = apply(Functor[F].map(fb)(\/-(_)))
  def left[A, F[_]: Applicative, B](a: A): EitherT[A, F, B] = apply(Applicative[F].point(-\/(a)))
  def right[A, F[_]: Applicative, B](b: B): EitherT[A, F, B] = apply(Applicative[F].point(\/-(b)))
  def pure[A, F[_]: Applicative, B](b: B): EitherT[A, F, B] = right(b)

  def fromDisjunction[F[_]]: FromDisjunctionAux[F] = new FromDisjunctionAux

  final class FromDisjunctionAux[F[_]] private[EitherT] {
    def apply[A, B](a: A \/ B)(implicit F: Applicative[F]): EitherT[A, F, B] =
      eitherT(F.point(a))
  }

  def eitherTU[FAB, AB, A0, B0](fab: FAB)(implicit
                                          u1: Unapply[Functor, FAB]{type A = AB},
                                          @deprecated("scala/bug#5075", "") u2: Unapply2[Bifunctor, AB]{type A = A0; type B = B0},
                                          l: AB === (A0 \/ B0)
  ): EitherT[A0, u1.M, B0] = eitherT(l.subst[u1.M](u1(fab)))

  def monadTell[F[_], W, A](implicit MT0: MonadTell[F, W]): EitherTMonadTell[F, W, A] = new EitherTMonadTell[F, W, A]{
    def MT = MT0
  }

  def monadListen[F[_], W, A](implicit ML0: MonadListen[F, W]): EitherTMonadListen[F, W, A] = new EitherTMonadListen[F, W, A]{
    def MT = ML0
  }

  def leftU[B]: EitherTLeft[B] =
    new EitherTLeft[B](true)

  /**
   * @example {{{
   * val a: String \/ Int = \/-(1)
   * val b: EitherT[({type l[a] = String \/ a})#l, Boolean, Int] = EitherT.rightU[Boolean](a)
   * }}}
   */
  def rightU[A]: EitherTRight[A] =
    new EitherTRight[A](true)

  private[scalaz] final class EitherTLeft[B](private val dummy: Boolean) extends AnyVal {
    def apply[FA](fa: FA)(implicit F: Unapply[Functor, FA]): EitherT[F.A, F.M, B] =
      leftT[F.A, F.M, B](F(fa))(F.TC)
  }

  private[scalaz] final class EitherTRight[A](private val dummy: Boolean) extends AnyVal {
    def apply[FB](fb: FB)(implicit F: Unapply[Functor, FB]): EitherT[A, F.M, F.A] =
      rightT[A, F.M, F.A](F(fb))(F.TC)
  }

  /** Construct a disjunction value from a standard `scala.Either`. */
  def fromEither[F[_], A, B](e: F[Either[A, B]])(implicit F: Functor[F]): EitherT[A, F, B] =
    apply(F.map(e)(_ fold (\/.left, \/.right)))

  /** Construct a disjunction value from a standard `scala.Option`. */
  def fromOption[F[_], A, B](ifNone: => A)(fo: F[Option[B]])(implicit F: Functor[F]): EitherT[A, F, B] =
    apply(F.map(fo)(o => \/.fromOption(ifNone)(o)))

  @deprecated("Throwable is not referentially transparent, use \\/.attempt", "7.3.0")
  def fromTryCatchThrowable[F[_], A, B <: Throwable: NotNothing](a: => F[A])(implicit F: Applicative[F], ex: ClassTag[B]): EitherT[B, F, A] =
    try {
      rightT(a)
    } catch {
      case e if ex.runtimeClass.isInstance(e) => leftT(F.point(e.asInstanceOf[B]))
    }

  @deprecated("Throwable is not referentially transparent, use \\/.attempt", "7.3.0")
  def fromTryCatchNonFatal[F[_], A](a: => F[A])(implicit F: Applicative[F]): EitherT[Throwable, F, A] =
    try {
      rightT(a)
    } catch {
      case NonFatal(t) => leftT(F.point(t))
    }

}

sealed abstract class EitherTInstances5 {
  implicit def eitherTNondeterminism[F[_], E](implicit F0: Nondeterminism[F]): Nondeterminism[EitherT[E, F, ?]] =
    new EitherTNondeterminism[F, E] {
      implicit def F = F0
    }
}

sealed abstract class EitherTInstances4 extends EitherTInstances5{
  implicit def eitherTBindRec[F[_], E](implicit F0: Monad[F], B0: BindRec[F]): BindRec[EitherT[E, F, ?]] =
    new EitherTBindRec[F, E] {
      implicit def F = F0
      implicit def B = B0
    }
}

sealed abstract class EitherTInstances3 extends EitherTInstances4 {
  implicit def eitherTFunctor[F[_], L](implicit F0: Functor[F]): Functor[EitherT[L, F, ?]] =
    new EitherTFunctor[F, L] {
      implicit def F = F0
    }
}

sealed abstract class EitherTInstances2 extends EitherTInstances3 {
  implicit def eitherTMonadError[F[_], E](implicit F0: Monad[F]): MonadError[EitherT[E, F, ?], E] =
    new EitherTMonadError[F, E] {
      implicit def F = F0
    }
}

sealed abstract class EitherTInstances1 extends EitherTInstances2 {
  implicit def eitherTPlus[F[_], L](implicit F0: Monad[F], L0: Semigroup[L]): Plus[EitherT[L, F, ?]] =
    new EitherTPlus[F, L] {
      implicit def F = F0
      implicit def G = L0
    }
  
  implicit def eitherTMonadReader[E, F[_], R](implicit F0: MonadReader[F, R]): MonadReader[EitherT[E, F, ?], R] = 
    new EitherTMonadReader[E, F, R] {
      implicit def F = F0
    }
}

sealed abstract class EitherTInstances0 extends EitherTInstances1 {
  implicit def eitherTBifunctor[F[_]](implicit F0: Functor[F]): Bifunctor[EitherT[?, F, ?]] =
    new EitherTBifunctor[F] {
      implicit def F = F0
    }
  implicit def eitherTBifoldable[F[_]](implicit F0: Foldable[F]): Bifoldable[EitherT[?, F, ?]] =
    new EitherTBifoldable[F] {
      implicit def F = F0
    }

  implicit def eitherTMonadPlusAlt[F[_], L](implicit F0: Monad[F], L0: Monoid[L]): MonadPlus[EitherT[L, F, ?]] with Alt[EitherT[L, F, ?]] =
    new EitherTMonadPlus[F, L] with Alt[EitherT[L, F, ?]] {
      implicit def F = F0
      implicit def G = L0

      def alt[A](a1: => EitherT[L, F, A], a2: => EitherT[L, F, A]): EitherT[L, F, A] =
        EitherT(
          F.bind(a1.run)(_ match {
            case -\/(l) =>
              F.map(a2.run)(
                _.leftMap(G.append(l, _))
              )

            case r: \/-[L, A] =>
              F.point(r)
          })
        )
    }

  implicit def eitherTFoldable[F[_], L](implicit F0: Foldable[F]): Foldable[EitherT[L, F, ?]] =
    new EitherTFoldable[F, L] {
      implicit def F = F0
    }
}

sealed abstract class EitherTInstances extends EitherTInstances0 {
  implicit def eitherTBitraverse[F[_]](implicit F0: Traverse[F]): Bitraverse[EitherT[?, F, ?]] =
    new EitherTBitraverse[F] {
      implicit def F = F0
    }

  implicit def eitherTTraverse[F[_], L](implicit F0: Traverse[F]): Traverse[EitherT[L, F, ?]] =
    new EitherTTraverse[F, L] {
      implicit def F = F0
    }

  implicit def eitherTHoist[A]: Hoist[λ[(α[_], β) => EitherT[A, α, β]]] =
    new EitherTHoist[A] {}

  implicit def eitherTEqual[F[_], A, B](implicit F0: Equal[F[A \/ B]]): Equal[EitherT[A, F, B]] =
    F0.contramap((_: EitherT[A, F, B]).run)

  implicit def eitherTShow[F[_], A, B](implicit F0: Show[F[A \/ B]]): Show[EitherT[A, F, B]] =
    Contravariant[Show].contramap(F0)(_.run)
}

private trait EitherTFunctor[F[_], E] extends Functor[EitherT[E, F, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: EitherT[E, F, A])(f: A => B): EitherT[E, F, B] = fa map f
}

private trait EitherTBind[F[_], E] extends Bind[EitherT[E, F, ?]] with EitherTFunctor[F, E] {
  implicit def F: Monad[F]

  final def bind[A, B](fa: EitherT[E, F, A])(f: A => EitherT[E, F, B]): EitherT[E, F, B] = fa flatMap f
}

private trait EitherTBindRec[F[_], E] extends BindRec[EitherT[E, F, ?]] with EitherTBind[F, E] {
  implicit def F: Monad[F]
  implicit def B: BindRec[F]

  final def tailrecM[A, B](a: A)(f: A => EitherT[E, F, A \/ B]): EitherT[E, F, B] =
    EitherT(
      B.tailrecM[A, E \/ B](a)(a => F.map(f(a).run) {
        // E \/ (A \/ B) => A \/ (E \/ B) is _.sequenceU but can't use here
        _.fold(e => \/-(-\/(e)), _.fold(\/.left, b => \/-(\/-(b))))
      })
    )
}

private trait EitherTMonad[F[_], E] extends Monad[EitherT[E, F, ?]] with EitherTBind[F, E] {
  implicit def F: Monad[F]

  def point[A](a: => A): EitherT[E, F, A] = EitherT(F.point(\/-(a)))
}

private trait EitherTPlus[F[_], E] extends Plus[EitherT[E, F, ?]] {
  def F: Monad[F]
  def G: Semigroup[E]

  def plus[A](a: EitherT[E, F, A], b: => EitherT[E, F, A]): EitherT[E, F, A] =
    EitherT(F.bind(a.run){
      case -\/(l) =>
        F.map(b.run){
          case -\/(ll)    => -\/(G.append(l, ll))
          case r @ \/-(_) => r
        }
      case r =>
        F.point(r)
    })
}

private trait EitherTMonadPlus[F[_], E] extends MonadPlus[EitherT[E, F, ?]] with EitherTMonad[F, E] with EitherTPlus[F, E] {
  def G: Monoid[E]

  def empty[A]: EitherT[E, F, A] = EitherT(F.point(-\/(G.zero)))
}

private trait EitherTFoldable[F[_], E] extends Foldable.FromFoldr[EitherT[E, F, ?]] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: EitherT[E, F, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

private trait EitherTTraverse[F[_], E] extends Traverse[EitherT[E, F, ?]] with EitherTFoldable[F, E] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: EitherT[E, F, A])(f: A => G[B]): G[EitherT[E, F, B]] = fa traverse f
}

private trait EitherTBifunctor[F[_]] extends Bifunctor[EitherT[?, F, ?]] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: EitherT[A, F, B])(f: A => C, g: B => D): EitherT[C, F, D] = fab.bimap(f, g)
}

private trait EitherTBifoldable[F[_]] extends Bifoldable.FromBifoldMap[EitherT[?, F, ?]] {
  implicit def F: Foldable[F]

  override final def bifoldMap[A, B, M: Monoid](fab: EitherT[A, F, B])(f: A => M)(g: B => M) =
    F.foldMap(fab.run)(Bifoldable[\/].bifoldMap(_)(f)(g))
}

private trait EitherTBitraverse[F[_]] extends Bitraverse[EitherT[?, F, ?]] with EitherTBifunctor[F] with EitherTBifoldable[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: EitherT[A, F, B])
                                                (f: A => G[C], g: B => G[D]): G[EitherT[C, F, D]] =
    fab.bitraverse(f, g)
}

private trait EitherTHoist[A] extends Hoist[λ[(α[_], β) => EitherT[A, α, β]]] {
  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]) =
    λ[EitherT[A, M, ?] ~> EitherT[A, N, ?]](_ mapT f.apply)

  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): EitherT[A, M, B] = EitherT(M.map(mb)(\/.right))

  implicit def apply[M[_] : Monad]: Monad[EitherT[A, M, ?]] = EitherT.eitherTMonadError
}

private[scalaz] trait EitherTMonadTell[F[_], W, A] extends MonadTell[EitherT[A, F, ?], W] with EitherTMonad[F, A] with EitherTHoist[A] {
  def MT: MonadTell[F, W]

  implicit def F = MT

  def writer[B](w: W, v: B): EitherT[A, F, B] =
    liftM[F, B](MT.writer(w, v))

  def left[B](v: => A): EitherT[A, F, B] =
    EitherT.leftT[A, F, B](MT.point(v))

  def right[B](v: => B): EitherT[A, F, B] =
    EitherT.rightT[A, F, B](MT.point(v))
}

private[scalaz] trait EitherTMonadListen[F[_], W, A] extends MonadListen[EitherT[A, F, ?], W] with EitherTMonadTell[F, W, A] {
  implicit def MT: MonadListen[F, W]

  def listen[B](ma: EitherT[A, F, B]): EitherT[A, F, (B, W)] = {
    val tmp = MT.bind[(A \/ B, W), A \/ (B, W)](MT.listen(ma.run)){
      case (a @ -\/(_), _) => MT.point(a.coerceRight)
      case (\/-(b), w) => MT.point(\/-((b, w)))
    }

    EitherT[A, F, (B, W)](tmp)
  }
}

private trait EitherTMonadError[F[_], E] extends MonadError[EitherT[E, F, ?], E] with EitherTMonad[F, E] {
  implicit def F: Monad[F]
  def raiseError[A](e: E): EitherT[E, F, A] = EitherT(F.point(-\/(e)))
  def handleError[A](fa: EitherT[E, F, A])(f: E => EitherT[E, F, A]): EitherT[E, F, A] =
    EitherT(F.bind(fa.run) {
      case -\/(e) => f(e).run
      case r => F.point(r)
    })
}

private trait EitherTNondeterminism[F[_], E] extends Nondeterminism[EitherT[E, F, ?]] with EitherTMonad[F, E] {
  implicit def F: Nondeterminism[F]

  def chooseAny[A](head: EitherT[E, F, A], tail: IList[EitherT[E, F, A]]): EitherT[E, F, (A, IList[EitherT[E, F, A]])] =
    EitherT(F.map(F.chooseAny(head.run, tail map (_.run))) {
      case (a, residuals) =>
        a.map((_, residuals.map(new EitherT(_))))
    })
}

private trait EitherTMonadReader[E,F[_],R] extends MonadReader[EitherT[E,F,?], R] with EitherTMonad[F, E] {
  implicit def F : MonadReader[F,R]
  
  def ask : EitherT[E,F,R] = EitherT.rightT(F.ask)
  def local[A](f: R => R)(fa: EitherT[E,F,A]): EitherT[E,F,A] = fa.mapT(e => F.local(f)(e))
}
