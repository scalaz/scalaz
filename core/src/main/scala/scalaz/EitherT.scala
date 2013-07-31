package scalaz

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

  sealed trait Switching_\/[X] {
    def r: X

    def <<?:(left: => X)(implicit F: Functor[F]): F[X] =
      F.map(EitherT.this.run){
        case -\/(_) => left
        case \/-(_) => r
      }
  }

  /** If this disjunction is right, return the given X value, otherwise, return the X value given to the return value. */
  def :?>>[X](right: => X): Switching_\/[X] =
    new Switching_\/[X] {
      def r = right
    }

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
    G.map(F.traverse(run)(o => Traverse[({type λ[α] = (A \/ α)})#λ].traverse(o)(f)))(EitherT(_))

  /** Run the side-effect on the right of this disjunction. */
  @deprecated("Each/foreach is deprecated", "7.1")
  def foreach(f: B => Unit)(implicit F: Each[F]): Unit =
    F.each(run)(_ foreach f)

  /** Apply a function in the environment of the right of this
    * disjunction.  Because it runs my `F` even when `f`'s `\/` fails,
    * it is not consistent with `ap`.
    */
  def app[C](f: => EitherT[F, A, B => C])(implicit F: Apply[F]): EitherT[F, A, C] =
    EitherT(F.apply2(f.run, run)((a, b) => b ap a))

  /** Bind through the right of this disjunction. */
  def flatMap[C](f: B => EitherT[F, A, C])(implicit F: Monad[F]): EitherT[F, A, C] =
    EitherT(F.bind(run)(_.fold(a => F.point(-\/(a): (A \/ C)), b => f(b).run)))

  /** Fold on the right of this disjunction. */
  def foldRight[Z](z: => Z)(f: (B, => Z) => Z)(implicit F: Foldable[F]): Z =
    F.foldRight[A \/ B, Z](run, z)((a, b) => a.foldRight(b)(f))

  /** Filter on the right of this disjunction. */
  def filter(p: B => Boolean)(implicit M: Monoid[A], F: Functor[F]): EitherT[F, A, B] =
    EitherT(F.map(run)(_.filter[A](p)))

  /** Alias for `filter`. */
  def withFilter(p: B => Boolean)(implicit M: Monoid[A], F: Functor[F]): EitherT[F, A, B] =
    filter(p)(M, F)

  /** Return `true` if this disjunction is a right value satisfying the given predicate. */
  def exists(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ exists f)

  /** Return `true` if this disjunction is a left value or the right value satisfies the given predicate. */
  def forall(f: B => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ forall f)

  /** Return an empty list or list with one element on the right of this disjunction. */
  def toList(implicit F: Functor[F]): F[List[B]] =
    F.map(run)(_.fold(_ => Nil, List(_)))

  /** Return an empty stream or stream with one element on the right of this disjunction. */
  def toStream(implicit F: Functor[F]): F[Stream[B]] =
    F.map(run)((_: (A \/ B)).fold(_ => Stream(), Stream(_)))

  /** Return an empty option or option with one element on the right of this disjunction. Useful to sweep errors under the carpet. */
  def toOption(implicit F: Functor[F]): OptionT[F, B] =
    optionT[F](F.map(run)((_: (A \/ B)).toOption))

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
  def orElse(x: => EitherT[F, A, B])(implicit F: Bind[F]): EitherT[F, A, B] = {
    val g = run
    EitherT(F.bind(g) {
      case -\/(_) => x.run
      case \/-(_) => g
    })
  }

  /** Return this if it is a right, otherwise, return the given value. Alias for `orElse` */
  def |||(x: => EitherT[F, A, B])(implicit F: Bind[F]): EitherT[F, A, B] =
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
}

object EitherT extends EitherTInstances with EitherTFunctions {
  /** Construct a left disjunction value. */
  def left[F[_], A, B](a: F[A])(implicit F: Functor[F]): EitherT[F, A, B] =
    apply(F.map(a)(\/.left(_)))

  /** Construct a right disjunction value. */
  def right[F[_], A, B](b: F[B])(implicit F: Functor[F]): EitherT[F, A, B] =
    apply(F.map(b)(\/.right(_)))

  /** Construct a disjunction value from a standard `scala.Either`. */
  def fromEither[F[_], A, B](e: F[Either[A, B]])(implicit F: Functor[F]): EitherT[F, A, B] =
    apply(F.map(e)(_ fold (\/.left, \/.right)))

  /** Evaluate the given value, which might throw an exception. */
  def fromTryCatch[F[_], A](a: => F[A])(implicit F: Applicative[F]): EitherT[F, Throwable, A] = try {
    right(a)
  } catch {
    case e: Throwable => left(F.point(e))
  }
}

sealed abstract class EitherTInstances1 {
  implicit def eitherTFunctor[F[_], L](implicit F0: Functor[F]) = new EitherTFunctor[F, L] {
    implicit def F = F0
  }
}

sealed abstract class EitherTInstances0 extends EitherTInstances1 {
  implicit def eitherTBifunctor[F[_]](implicit F0: Functor[F]) = new EitherTBifunctor[F] {
    implicit def F = F0
  }

  implicit def eitherTMonad[F[_], L](implicit F0: Monad[F]) = new EitherTMonad[F, L] {
    implicit def F = F0
  }
  implicit def eitherTFoldable[F[_], L](implicit F0: Foldable[F]) = new EitherTFoldable[F, L] {
    implicit def F = F0
  }
}

sealed abstract class EitherTInstances extends EitherTInstances0 {
  implicit def eitherTBitraverse[F[_]](implicit F0: Traverse[F]) = new EitherTBitraverse[F] {
    implicit def F = F0
  }

  implicit def eitherTTraverse[F[_], L](implicit F0: Traverse[F]) = new EitherTTraverse[F, L] {
    implicit def F = F0
  }

  implicit def eitherTHoist[A]: Hoist[({type λ[α[_], β] = EitherT[α, A, β]})#λ] = new EitherTHoist[A] {}

  implicit def eitherTEqual[F[_], A, B](implicit F0: Equal[F[A \/ B]]): Equal[EitherT[F, A, B]] = F0.contramap((_: EitherT[F, A, B]).run)
}

trait EitherTFunctions {
  def eitherT[F[_], A, B](a: F[A \/ B]): EitherT[F, A, B] = EitherT[F, A, B](a)

  def monadTell[F[_, _], W, A](implicit MT0: MonadTell[F, W]) = new EitherTMonadTell[F, W, A]{
    def MT = MT0
  }

  def monadListen[F[_, _], W, A](implicit ML0: MonadListen[F, W]) = new EitherTMonadListen[F, W, A]{
    def MT = ML0
  }
}


private[scalaz] trait EitherTFunctor[F[_], E] extends Functor[({type λ[α]=EitherT[F, E, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: EitherT[F, E, A])(f: A => B): EitherT[F, E, B] = fa map f
}

private[scalaz] trait EitherTMonad[F[_], E] extends Monad[({type λ[α]=EitherT[F, E, α]})#λ] with EitherTFunctor[F, E] {
  implicit def F: Monad[F]

  def point[A](a: => A): EitherT[F, E, A] = EitherT(F.point(\/-(a)))

  def bind[A, B](fa: EitherT[F, E, A])(f: A => EitherT[F, E, B]): EitherT[F, E, B] = fa flatMap f
}

private[scalaz] trait EitherTFoldable[F[_], E] extends Foldable.FromFoldr[({type λ[α]=EitherT[F, E, α]})#λ] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: EitherT[F, E, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

private[scalaz] trait EitherTTraverse[F[_], E] extends Traverse[({type λ[α]=EitherT[F, E, α]})#λ] with EitherTFoldable[F, E] {
  implicit def F: Traverse[F]

  def traverseImpl[G[_]: Applicative, A, B](fa: EitherT[F, E, A])(f: A => G[B]): G[EitherT[F, E, B]] = fa traverse f
}

private[scalaz] trait EitherTBifunctor[F[_]] extends Bifunctor[({type λ[α, β]=EitherT[F, α, β]})#λ] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: EitherT[F, A, B])(f: A => C, g: B => D): EitherT[F, C, D] = fab.bimap(f, g)
}

private[scalaz] trait EitherTBitraverse[F[_]] extends Bitraverse[({type λ[α, β] = EitherT[F, α, β]})#λ] with EitherTBifunctor[F] {
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: EitherT[F, A, B])
                                                (f: A => G[C], g: B => G[D]): G[EitherT[F, C, D]] =
    fab.bitraverse(f, g)
}

private[scalaz] trait EitherTHoist[A] extends Hoist[({type λ[α[_], β] = EitherT[α, A, β]})#λ] {
  def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]) = new (({type λ[α] = EitherT[M, A, α]})#λ ~> ({type λ[α] = EitherT[N, A, α]})#λ) {
    def apply[B](mb: EitherT[M, A, B]): EitherT[N, A, B] = EitherT(f.apply(mb.run))
  }

  def liftM[M[_], B](mb: M[B])(implicit M: Monad[M]): EitherT[M, A, B] = EitherT(M.map(mb)(\/-(_)))

  implicit def apply[M[_] : Monad]: Monad[({type λ[α] = EitherT[M, A, α]})#λ] = EitherT.eitherTMonad
}

private[scalaz] trait EitherTMonadTell[F[_, _], W, A] extends MonadTell[({type λ[α, β] = EitherT[({type f[x] = F[α, x]})#f, A, β]})#λ, W] with EitherTMonad[({type λ[α] = F[W, α]})#λ, A] with EitherTHoist[A] {
  def MT: MonadTell[F, W]

  implicit def F = MT

  def writer[B](w: W, v: B): EitherT[({type λ[α] = F[W, α]})#λ, A, B] =
    liftM[({type λ[α] = F[W, α]})#λ, B](MT.writer(w, v))

  def left[B](v: => A): EitherT[({type λ[α] = F[W, α]})#λ, A, B] =
    EitherT.left[({type λ[α] = F[W, α]})#λ, A, B](MT.point(v))

  def right[B](v: => B): EitherT[({type λ[α] = F[W, α]})#λ, A, B] =
    EitherT.right[({type λ[α] = F[W, α]})#λ, A, B](MT.point(v))
}

private[scalaz] trait EitherTMonadListen[F[_, _], W, A] extends MonadListen[({type λ[α, β] = EitherT[({type f[x] = F[α, x]})#f, A, β]})#λ, W] with EitherTMonadTell[F, W, A] {
  implicit def MT: MonadListen[F, W]

  def listen[B](ma: EitherT[({type λ[α] = F[W, α]})#λ, A, B]): EitherT[({type λ[α] = F[W, α]})#λ, A, (B, W)] = {
    val tmp = MT.bind[(A \/ B, W), A \/ (B, W)](MT.listen(ma.run)){
      case (-\/(a), _) => MT.point(-\/(a))
      case (\/-(b), w) => MT.point(\/-((b, w)))
    }

    EitherT[({type λ[α] = F[W, α]})#λ, A, (B, W)](tmp)
  }
}
