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
final case class EitherT[F[_], A, B](run: F[A \/ B]) {
  import OptionT._

  final class Switching_\/[X](r: => X) {
    def <<?:(left: => X)(implicit F: Functor[F]): F[X] =
      F.map(EitherT.this.run){
        case -\/(_) => left
        case \/-(_) => r
      }
  }

  /** If this disjunction is right, return the given X value, otherwise, return the X value given to the return value. */
  def :?>>[X](right: => X): Switching_\/[X] =
    new Switching_\/(right)

  def fold[X](l: A => X, r: B => X)(implicit F: Functor[F]): F[X] =
    F.map(run)(_.fold(l, r))

  /** Return `true` if this disjunction is left. */
  def isLeft(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isLeft)

  /** Return `true` if this disjunction is right. */
  def isRight(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isRight)

  /** Flip the left/right values in this disjunction. Alias for `swap` */
  def swap(implicit F: Functor[F]): EitherT[F, B, A] =
    EitherT(F.map(run)(_.swap))

  /** Flip the left/right values in this disjunction. Alias for `unary_~` */
  def unary_~(implicit F: Functor[F]): EitherT[F, B, A] =
    swap

  /** Run the given function on this swapped value. Alias for `~` */
  def swapped[AA, BB](k: (B \/ A) => (BB \/ AA))(implicit F: Functor[F]): EitherT[F, AA, BB] =
    EitherT(F.map(run)(_ swapped k))

  /** Run the given function on this swapped value. Alias for `swapped` */
  def ~[AA, BB](k: (B \/ A) => (BB \/ AA))(implicit F: Functor[F]): EitherT[F, AA, BB] =
    swapped(k)

  /** Binary functor map on this disjunction. */
  def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): EitherT[F, C, D] =
    EitherT(F.map(run)(_.bimap(f, g)))

  /** Run the given function on the left value. */
  def leftMap[C](f: A => C)(implicit F: Functor[F]): EitherT[F, C, B] =
    bimap(f, identity)

  /** Binary functor traverse on this disjunction. */
  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit F: Traverse[F], G: Applicative[G]): G[EitherT[F, C, D]] =
    Applicative[G].map(F.traverse(run)(Bitraverse[\/].bitraverseF(f, g)))(EitherT(_: F[C \/ D]))

  /** Map on the right of this disjunction. */
  def map[C](f: B => C)(implicit F: Functor[F]): EitherT[F, A, C] =
    EitherT(F.map(run)(_.map(f)))

  /** Traverse on the right of this disjunction. */
  def traverse[G[_], C](f: B => G[C])(implicit F: Traverse[F], G: Applicative[G]): G[EitherT[F, A, C]] =
    G.map(F.traverse(run)(o => Traverse[A \/ ?].traverse(o)(f)))(EitherT(_))

  /** Apply a function in the environment of the right of this
    * disjunction.  Because it runs my `F` even when `f`'s `\/` fails,
    * it is not consistent with `ap`.
    */
  def app[C](f: => EitherT[F, A, B => C])(implicit F: Apply[F]): EitherT[F, A, C] =
    EitherT(F.apply2(f.run, run)((a, b) => b ap a))

  /** Bind through the right of this disjunction. */
  def flatMap[C](f: B => EitherT[F, A, C])(implicit F: Monad[F]): EitherT[F, A, C] =
    EitherT(F.bind(run)(_.fold(a => F.point(-\/(a): (A \/ C)), b => f(b).run)))

  /** Bind the inner monad through the right of this disjunction. */
  def flatMapF[C](f: B => F[A \/ C])(implicit F: Monad[F]): EitherT[F, A, C] =
    EitherT(F.bind(run)(_.fold(a => F.point(-\/(a): (A \/ C)), f)))

  /** Fold on the right of this disjunction. */
  def foldRight[Z](z: => Z)(f: (B, => Z) => Z)(implicit F: Foldable[F]): Z =
    F.foldRight[A \/ B, Z](run, z)((a, b) => a.foldRight(b)(f))

  /** Filter on the right of this disjunction. */
  def filter(p: B => Boolean)(implicit M: Monoid[A], F: Monad[F]): EitherT[F, A, B] =
    MonadPlus[EitherT[F, A, ?]].filter(this)(p)

  /** Alias for `filter`.
   */
  def withFilter(p: B => Boolean)(implicit M: Monoid[A], F: Monad[F]): EitherT[F, A, B] =
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
  def orElse(x: => EitherT[F, A, B])(implicit F: Monad[F]): EitherT[F, A, B] = {
    val g = run
    EitherT(F.bind(g) {
      case    -\/(_)  => x.run
      case r@(\/-(_)) => F.point(r)
    })
  }

  /** Return this if it is a right, otherwise, return the given value. Alias for `orElse` */
  def |||(x: => EitherT[F, A, B])(implicit F: Monad[F]): EitherT[F, A, B] =
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
  def +++(x: => EitherT[F, A, B])(implicit M1: Semigroup[B], M2: Semigroup[A], F: Apply[F]): EitherT[F, A, B] =
    EitherT(F.apply2(run, x.run)(_ +++ _))

  /** Ensures that the right value of this disjunction satisfies the given predicate, or returns left with the given value. */
  def ensure(onLeft: => A)(f: B => Boolean)(implicit F: Functor[F]): EitherT[F, A, B] =
    EitherT(F.map(run)(_.ensure(onLeft)(f)))

  /** Compare two disjunction values for equality. */
  def ===(x: EitherT[F, A, B])(implicit EA: Equal[A], EB: Equal[B], F: Apply[F]): F[Boolean] =
    F.apply2(run, x.run)(_ === _)

  /** Compare two disjunction values for ordering. */
  def compare(x: EitherT[F, A, B])(implicit EA: Order[A], EB: Order[B], F: Apply[F]): F[Ordering] =
    F.apply2(run, x.run)(_ compare _)

  /** Show for a disjunction value. */
  def show(implicit SA: Show[A], SB: Show[B], F: Functor[F]): F[Cord] =
    F.map(run)(_.show[A, B])

  /** Cozip this disjunction on its functor. */
  def cozip(implicit Z: Cozip[F]): (F[A] \/ F[B]) =
    Z.cozip(run)

  /** Convert to a validation. */
  def validation(implicit F: Functor[F]): F[Validation[A, B]] =
    F.map(run)(_.validation)

  /** Run a validation function and back to disjunction again. */
  def validationed[AA, BB](k: Validation[A, B] => Validation[AA, BB])(implicit F: Functor[F]): EitherT[F, AA, BB] =
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

  def eitherT[F[_], A, B](a: F[A \/ B]): EitherT[F, A, B] = EitherT[F, A, B](a)

  def fromDisjunction[F[_]]: FromDisjunctionAux[F] = new FromDisjunctionAux

  final class FromDisjunctionAux[F[_]] private[EitherT] {
    def apply[A, B](a: A \/ B)(implicit F: Applicative[F]): EitherT[F, A, B] =
      eitherT(F.point(a))
  }

  def eitherTU[FAB, AB, A0, B0](fab: FAB)(
    implicit u1: Unapply[Functor, FAB]{type A = AB}, u2: Unapply2[Bifunctor, AB]{type A = A0; type B = B0}, l: Leibniz.===[AB, A0 \/ B0])
      : EitherT[u1.M, A0, B0] = eitherT(l.subst[u1.M](u1(fab)))

  def monadTell[F[_], W, A](implicit MT0: MonadTell[F, W]): EitherTMonadTell[F, W, A] = new EitherTMonadTell[F, W, A]{
    def MT = MT0
  }

  def monadListen[F[_], W, A](implicit ML0: MonadListen[F, W]): EitherTMonadListen[F, W, A] = new EitherTMonadListen[F, W, A]{
    def MT = ML0
  }

  /** Construct a left disjunction value. */
  def left[F[_], A, B](a: F[A])(implicit F: Functor[F]): EitherT[F, A, B] =
    apply(F.map(a)(\/.left))

  /** Construct a right disjunction value. */
  def right[F[_], A, B](b: F[B])(implicit F: Functor[F]): EitherT[F, A, B] =
    apply(F.map(b)(\/.right))

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

  private[scalaz] final class EitherTLeft[B](val dummy: Boolean) extends AnyVal {
    def apply[FA](fa: FA)(implicit F: Unapply[Functor, FA]): EitherT[F.M, F.A, B] =
      left[F.M, F.A, B](F(fa))(F.TC)
  }

  private[scalaz] final class EitherTRight[A](val dummy: Boolean) extends AnyVal {
    def apply[FB](fb: FB)(implicit F: Unapply[Functor, FB]): EitherT[F.M, A, F.A] =
      right[F.M, A, F.A](F(fb))(F.TC)
  }

  /** Construct a disjunction value from a standard `scala.Either`. */
  def fromEither[F[_], A, B](e: F[Either[A, B]])(implicit F: Functor[F]): EitherT[F, A, B] =
    apply(F.map(e)(_ fold (\/.left, \/.right)))

  def fromTryCatchThrowable[F[_], A, B <: Throwable](a: => F[A])(implicit F: Applicative[F], nn: NotNothing[B], ex: ClassTag[B]): EitherT[F, B, A] =
    try {
      right(a)
    } catch {
      case e if ex.runtimeClass.isInstance(e) => left(F.point(e.asInstanceOf[B]))
    }

  def fromTryCatchNonFatal[F[_], A](a: => F[A])(implicit F: Applicative[F]): EitherT[F, Throwable, A] =
    try {
      right(a)
    } catch {
      case NonFatal(t) => left(F.point(t))
    }

}

sealed abstract class EitherTInstances4 {
  implicit def eitherTBindRec[F[_], E](implicit F0: Monad[F], B0: BindRec[F]): BindRec[EitherT[F, E, ?]] =
    new EitherTBindRec[F, E] {
      implicit def F = F0
      implicit def B = B0
    }
}

sealed abstract class EitherTInstances3 extends EitherTInstances4 {
  implicit def eitherTMonadError[F[_], E](implicit F0: Monad[F]): MonadError[EitherT[F, E, ?], E] =
    new EitherTMonadError[F, E] {
      implicit def F = F0
    }
}

sealed abstract class EitherTInstances2 extends EitherTInstances3 {
  implicit def eitherTFunctor[F[_], L](implicit F0: Functor[F]): Functor[EitherT[F, L, ?]] =
    new EitherTFunctor[F, L] {
      implicit def F = F0
    }
}

sealed abstract class EitherTInstances1 extends EitherTInstances2 {
  implicit def eitherTMonad[F[_], L](implicit F0: Monad[F]): Monad[EitherT[F, L, ?]] =
    new EitherTMonad[F, L] {
      implicit def F = F0
    }
  implicit def eitherTPlus[F[_], L](implicit F0: Monad[F], L0: Semigroup[L]): Plus[EitherT[F, L, ?]] =
    new EitherTPlus[F, L] {
      implicit def F = F0
      implicit def G = L0
    }
}

sealed abstract class EitherTInstances0 extends EitherTInstances1 {
  implicit def eitherTBifunctor[F[_]](implicit F0: Functor[F]): Bifunctor[EitherT[F, ?, ?]] =
    new EitherTBifunctor[F] {
      implicit def F = F0
    }
  implicit def eitherTBifoldable[F[_]](implicit F0: Foldable[F]): Bifoldable[EitherT[F, ?, ?]] =
    new EitherTBifoldable[F] {
      implicit def F = F0
    }
  implicit def eitherTMonadPlus[F[_], L](implicit F0: Monad[F], L0: Monoid[L]): MonadPlus[EitherT[F, L, ?]] =
    new EitherTMonadPlus[F, L] {
      implicit def F = F0
      implicit def G = L0
    }
  implicit def eitherTFoldable[F[_], L](implicit F0: Foldable[F]): Foldable[EitherT[F, L, ?]] =
    new EitherTFoldable[F, L] {
      implicit def F = F0
    }
}

sealed abstract class EitherTInstances extends EitherTInstances0 {
  implicit def eitherTBitraverse[F[_]](implicit F0: Traverse[F]): Bitraverse[EitherT[F, ?, ?]] =
    new EitherTBitraverse[F] {
      implicit def F = F0
    }

  implicit def eitherTTraverse[F[_], L](implicit F0: Traverse[F]): Traverse[EitherT[F, L, ?]] =
    new EitherTTraverse[F, L] {
      implicit def F = F0
    }

  implicit def eitherTHoist[A]: Hoist[λ[(α[_], β) => EitherT[α, A, β]]] =
    new EitherTHoist[A] {}

  implicit def eitherTEqual[F[_], A, B](implicit F0: Equal[F[A \/ B]]): Equal[EitherT[F, A, B]] =
    F0.contramap((_: EitherT[F, A, B]).run)

  implicit def eitherTShow[F[_], A, B](implicit F0: Show[F[A \/ B]]): Show[EitherT[F, A, B]] =
    Contravariant[Show].contramap(F0)(_.run)
}

private trait EitherTFunctor[F[_], E] extends Functor[EitherT[F, E, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: EitherT[F, E, A])(f: A => B): EitherT[F, E, B] = fa map f
}

private trait EitherTBind[F[_], E] extends Bind[EitherT[F, E, ?]] with EitherTFunctor[F, E] {
  implicit def F: Monad[F]

  final def bind[A, B](fa: EitherT[F, E, A])(f: A => EitherT[F, E, B]): EitherT[F, E, B] = fa flatMap f
}

private trait EitherTBindRec[F[_], E] extends BindRec[EitherT[F, E, ?]] with EitherTBind[F, E] {
  implicit def F: Monad[F]
  implicit def B: BindRec[F]

  final def tailrecM[A, B](f: A => EitherT[F, E, A \/ B])(a: A): EitherT[F, E, B] =
    EitherT(
      B.tailrecM[A, E \/ B](a => F.map(f(a).run) {
        // E \/ (A \/ B) => A \/ (E \/ B) is _.sequenceU but can't use here
        _.fold(e => \/-(-\/(e)), _.fold(\/.left, b => \/-(\/-(b))))
      })(a)
    )
}

private trait EitherTMonad[F[_], E] extends Monad[EitherT[F, E, ?]] with EitherTBind[F, E] {
  implicit def F: Monad[F]

  def point[A](a: => A): EitherT[F, E, A] = EitherT(F.point(\/-(a)))
}

private trait EitherTPlus[F[_], E] extends Plus[EitherT[F, E, ?]] {
  def F: Monad[F]
  def G: Semigroup[E]

  def plus[A](a: EitherT[F, E, A], b: => EitherT[F, E, A]): EitherT[F, E, A] =
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

private trait EitherTMonadPlus[F[_], E] extends MonadPlus[EitherT[F, E, ?]] with EitherTMonad[F, E] with EitherTPlus[F, E] {
  def G: Monoid[E]

  def empty[A]: EitherT[F, E, A] = EitherT(F.point(-\/(G.zero)))
}

private trait EitherTFoldable[F[_], E] extends Foldable.FromFoldr[EitherT[F, E, ?]] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: EitherT[F, E, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

private trait EitherTTraverse[F[_], E] extends Traverse[EitherT[F, E, ?]] with EitherTFoldable[F, E] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: EitherT[F, E, A])(f: A => G[B]): G[EitherT[F, E, B]] = fa traverse f
}

private trait EitherTBifunctor[F[_]] extends Bifunctor[EitherT[F, ?, ?]] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: EitherT[F, A, B])(f: A => C, g: B => D): EitherT[F, C, D] = fab.bimap(f, g)
}

private trait EitherTBifoldable[F[_]] extends Bifoldable.FromBifoldMap[EitherT[F, ?, ?]] {
  implicit def F: Foldable[F]

  override final def bifoldMap[A, B, M: Monoid](fab: EitherT[F, A, B])(f: A => M)(g: B => M) =
    F.foldMap(fab.run)(Bifoldable[\/].bifoldMap(_)(f)(g))
}

private trait EitherTBitraverse[F[_]] extends Bitraverse[EitherT[F, ?, ?]] with EitherTBifunctor[F] with EitherTBifoldable[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: EitherT[F, A, B])
                                                (f: A => G[C], g: B => G[D]): G[EitherT[F, C, D]] =
    fab.bitraverse(f, g)
}

private trait EitherTHoist[A] extends Hoist[λ[(α[_], β) => EitherT[α, A, β]]] {
  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]) = new (EitherT[M, A, ?] ~> EitherT[N, A, ?]) {
    def apply[B](mb: EitherT[M, A, B]): EitherT[N, A, B] = EitherT(f.apply(mb.run))
  }

  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): EitherT[M, A, B] = EitherT(M.map(mb)(\/.right))

  implicit def apply[M[_] : Monad]: Monad[EitherT[M, A, ?]] = EitherT.eitherTMonad
}

private[scalaz] trait EitherTMonadTell[F[_], W, A] extends MonadTell[EitherT[F, A, ?], W] with EitherTMonad[F, A] with EitherTHoist[A] {
  def MT: MonadTell[F, W]

  implicit def F = MT

  def writer[B](w: W, v: B): EitherT[F, A, B] =
    liftM[F, B](MT.writer(w, v))

  def left[B](v: => A): EitherT[F, A, B] =
    EitherT.left[F, A, B](MT.point(v))

  def right[B](v: => B): EitherT[F, A, B] =
    EitherT.right[F, A, B](MT.point(v))
}

private[scalaz] trait EitherTMonadListen[F[_], W, A] extends MonadListen[EitherT[F, A, ?], W] with EitherTMonadTell[F, W, A] {
  implicit def MT: MonadListen[F, W]

  def listen[B](ma: EitherT[F, A, B]): EitherT[F, A, (B, W)] = {
    val tmp = MT.bind[(A \/ B, W), A \/ (B, W)](MT.listen(ma.run)){
      case (-\/(a), _) => MT.point(-\/(a))
      case (\/-(b), w) => MT.point(\/-((b, w)))
    }

    EitherT[F, A, (B, W)](tmp)
  }
}

private trait EitherTMonadError[F[_], E] extends MonadError[EitherT[F, E, ?], E] with EitherTMonad[F, E] {
  implicit def F: Monad[F]
  def raiseError[A](e: E): EitherT[F, E, A] = EitherT(F.point(-\/(e)))
  def handleError[A](fa: EitherT[F, E, A])(f: E => EitherT[F, E, A]): EitherT[F, E, A] =
    EitherT(F.bind(fa.run) {
      case -\/(e) => f(e).run
      case r => F.point(r)
    })
}
