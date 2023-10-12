package scalaz

////
/**
 * A type parameter implying the ability to extract zero or more
 * values of that type.
 */
////
trait Foldable[F[_]] extends FoldableParent[F] { self =>
  ////
  import scala.collection.generic.CanBuildFrom

  /** Map each element of the structure to a [[scalaz.Monoid]], and combine the results. */
  def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B
  /** As `foldMap` but returning `None` if the foldable is empty and `Some` otherwise */
  def foldMap1Opt[A,B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): Option[B] = {
    import std.option._
    foldMap(fa)(x => some(f(x)))
  }

  /**Right-associative fold of a structure. */
  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

  /**The composition of Foldables `F` and `G`, `[x]F[G[x]]`, is a Foldable */
  def compose[G[_]](implicit G0: Foldable[G]): Foldable[λ[α => F[G[α]]]] =
    new CompositionFoldable[F, G] {
      override def F = self
      override def G = G0
    }

  /** The composition of Foldable `F` and Bifoldable `G`, `[x, y]F[G[x, y]]`, is a Bifoldable */
  def bicompose[G[_, _]: Bifoldable]: Bifoldable[λ[(α, β) => F[G[α, β]]]] =
    new CompositionFoldableBifoldable[F, G] {
      def F = self
      def G = implicitly
    }

  /**The product of Foldables `F` and `G`, `[x](F[x], G[x]])`, is a Foldable */
  def product[G[_]](implicit G0: Foldable[G]): Foldable[λ[α => (F[α], G[α])]] =
    new ProductFoldable[F, G] {
      override def F = self
      override def G = G0
    }

  /**The product of Foldable `F` and Foldable1 `G`, `[x](F[x], G[x]])`, is a Foldable1 */
  def product0[G[_]](implicit G0: Foldable1[G]): Foldable1[λ[α => (F[α], G[α])]] =
    new ProductFoldable1R[F, G] {
      def F = self
      def G = G0
    }

  /**Left-associative fold of a structure. */
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B = {
    import Dual._, Endo._, syntax.std.all._
    Tag.unwrap(foldMap(fa)((a: A) => Dual(Endo.endo(f.flip.curried(a))))(dualMonoid)) apply (z)
  }

  /**Right-associative, monadic fold of a structure. */
  def foldRightM[G[_], A, B](fa: F[A], z: => B)(f: (A, => B) => G[B])(implicit M: Monad[G]): G[B] =
    foldLeft[A, B => G[B]](fa, M.point(_))((b, a) => w => M.bind(f(a, w))(b))(z)

  /**Left-associative, monadic fold of a structure. */
  def foldLeftM[G[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit M: Monad[G]): G[B] =
    foldRight[A, B => G[B]](fa, M.point(_))((a, b) => w => M.bind(f(w, a))(b))(z)

  /** Specialization of foldRightM when `B` has a `Monoid`. */
  def foldMapM[G[_], A, B](fa: F[A])(f: A => G[B])(implicit B: Monoid[B], G: Monad[G]): G[B] =
    foldRightM[G, A, B](fa, B.zero)((a, b2) => G.map(f(a))(b1 => B.append(b1, b2)))

  /** Combine the elements of a structure using a monoid. */
  def fold[M: Monoid](t: F[M]): M = foldMap[M, M](t)(x => x)

  /** Strict traversal in an applicative functor `M` that ignores the result of `f`. */
  def traverse_[M[_], A, B](fa: F[A])(f: A => M[B])(implicit a: Applicative[M]): M[Unit] =
    foldLeft(fa, a.pure(()))((x, y) => a.ap(f(y))(a.map(x)(_ => _ => ())))

  /** A version of `traverse_` that infers the type constructor `M`. */
  final def traverseU_[A, GB](fa: F[A])(f: A => GB)(implicit G: Unapply[Applicative, GB]): G.M[Unit] =
    traverse_[G.M, A, G.A](fa)(G.leibniz.onF(f))(G.TC)

  /** `traverse_` specialized to `State` **/
  def traverseS_[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, Unit] =
    State{ s =>
      (foldLeft(fa, s)((s, a) => f(a)(s)._1), ())
    }

  /** Strict sequencing in an applicative functor `M` that ignores the value in `fa`. */
  def sequence_[M[_], A](fa: F[M[A]])(implicit a: Applicative[M]): M[Unit] =
    traverse_(fa)(x => x)

  /** `sequence_` specialized to `State` **/
  def sequenceS_[S, A](fga: F[State[S, A]]): State[S, Unit] =
    traverseS_(fga)(x => x)

  /** `sequence_` for Free. collapses into a single Free **/
  def sequenceF_[M[_], A](ffa: F[Free[M, A]]): Free[M, Unit] =
    foldLeft[Free[M,A],Free[M,Unit]](ffa, Free.pure[M, Unit](()))((c,d) => c.flatMap(_ => d.map(_ => ())))

  /**Curried version of `foldRight` */
  final def foldr[A, B](fa: F[A], z: => B)(f: A => (=> B) => B): B = foldRight(fa, z)((a, b) => f(a)(b))
  def foldMapRight1Opt[A, B](fa: F[A])(z: A => B)(f: (A, => B) => B): Option[B] =
    foldRight(fa, None: Option[B])((a, optB) =>
      optB map (f(a, _)) orElse Some(z(a)))
  def foldRight1Opt[A](fa: F[A])(f: (A, => A) => A): Option[A] =
    foldMapRight1Opt(fa)(identity)(f)
  def foldr1Opt[A](fa: F[A])(f: A => (=> A) => A): Option[A] = foldRight(fa, None: Option[A])((a, optA) => optA map (aa => f(a)(aa)) orElse Some(a))

  /**Curried version of `foldLeft` */
  final def foldl[A, B](fa: F[A], z: B)(f: B => A => B): B = foldLeft(fa, z)((b, a) => f(b)(a))
  def foldMapLeft1Opt[A, B](fa: F[A])(z: A => B)(f: (B, A) => B): Option[B] =
    foldLeft(fa, None: Option[B])((optB, a) =>
      optB map (f(_, a)) orElse Some(z(a)))
  def foldLeft1Opt[A](fa: F[A])(f: (A, A) => A): Option[A] =
    foldMapLeft1Opt(fa)(identity)(f)
  def foldl1Opt[A](fa: F[A])(f: A => A => A): Option[A] = foldLeft(fa, None: Option[A])((optA, a) => optA map (aa => f(aa)(a)) orElse Some(a))

  /**Curried version of `foldRightM` */
  final def foldrM[G[_], A, B](fa: F[A], z: => B)(f: A => ( => B) => G[B])(implicit M: Monad[G]): G[B] =
    foldRightM(fa, z)((a, b) => f(a)(b))

  /**Curried version of `foldLeftM` */
  final def foldlM[G[_], A, B](fa: F[A], z: => B)(f: B => A => G[B])(implicit M: Monad[G]): G[B] =
    foldLeftM(fa, z)((b, a) => f(b)(a))

  /** map elements in a Foldable with a monadic function and return the first element that is mapped successfully */
  final def findMapM[M[_]: Monad, A, B](fa: F[A])(f: A => M[Option[B]]): M[Option[B]] =
    toEphemeralStream(fa) findMapM f

  def findLeft[A](fa: F[A])(f: A => Boolean): Option[A] =
    foldLeft[A, Option[A]](fa, None)((b, a) => b.orElse(if(f(a)) Some(a) else None))

  def findRight[A](fa: F[A])(f: A => Boolean): Option[A] =
    foldRight[A, Option[A]](fa, None)((a, b) => b.orElse(if(f(a)) Some(a) else None))

  /** Alias for `length`. */
  final def count[A](fa: F[A]): Int = length(fa)

  /** Deforested alias for `toStream(fa).size`. */
  def length[A](fa: F[A]): Int = foldLeft(fa, 0)((b, _) => b + 1)

  /**
   * @return the element at index `i` in a `Some`, or `None` if the given index falls outside of the range
   */
  def index[A](fa: F[A], i: Int): Option[A] =
    foldLeft[A, (Int, Option[A])](fa, (0, None)) {
      case ((idx, elem), curr) =>
        (idx + 1, elem orElse { if (idx == i) Some(curr) else None })
    }._2

  /**
   * @return the element at index `i`, or `default` if the given index falls outside of the range
   */
  def indexOr[A](fa: F[A], default: => A, i: Int): A =
    index(fa, i) getOrElse default

  def toList[A](fa: F[A]): List[A] = {
    foldLeft(fa, List.newBuilder[A]){ (buf, a) => buf += a }.result()
  }
  def toVector[A](fa: F[A]): Vector[A] = {
    foldLeft(fa, Vector.newBuilder[A]){ (buf, a) => buf += a }.result()
  }
  def toSet[A](fa: F[A]): Set[A] = {
    foldLeft(fa, Set.newBuilder[A]){ (buf, a) => buf += a }.result()
  }
  def toStream[A](fa: F[A]): Stream[A] = foldRight[A, Stream[A]](fa, Stream.empty)(Stream.cons(_, _))
  /**
    * @throws NoSuchMethodException if Scala 2.13 or later
    */
  @deprecated(message = "removed in scalaz 7.3", since = "7.2.22")
  def to[A, G[_]](fa: F[A])(implicit c: CanBuildFrom[Nothing, A, G[A]]): G[A] = {
    import scala.language.reflectiveCalls
    val builder = c.asInstanceOf[{def apply(): scala.collection.mutable.Builder[A, G[A]]}].apply()
    foldLeft(fa, builder)(_ += _).result
  }

  def toIList[A](fa: F[A]): IList[A] =
    foldLeft(fa, IList.empty[A])((t, h) => h :: t).reverse

  def toEphemeralStream[A](fa: F[A]): EphemeralStream[A] =
    foldRight(fa, EphemeralStream.emptyEphemeralStream[A])(EphemeralStream.cons(_, _))

  /** Whether all `A`s in `fa` yield true from `p`. */
  def all[A](fa: F[A])(p: A => Boolean): Boolean = foldRight(fa, true)(p(_) && _)
  /** `all` with monadic traversal. */
  def allM[G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] =
    foldRight(fa, G.point(true))((a, b) => G.bind(p(a))(q => if(q) b else G.point(false)))
  /** Whether any `A`s in `fa` yield true from `p`. */
  def any[A](fa: F[A])(p: A => Boolean): Boolean = foldRight(fa, false)(p(_) || _)
  /** `any` with monadic traversal. */
  def anyM[G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] =
    foldRight(fa, G.point(false))((a, b) => G.bind(p(a))(q => if(q) G.point(true) else b))

  import Ordering.{GT, LT}
  import std.option.{some, none}

  /** The greatest element of `fa`, or None if `fa` is empty. */
  def maximum[A: Order](fa: F[A]): Option[A] =
    foldLeft(fa, none[A]) {
      case (None, y) => some(y)
      case (Some(x), y) => some(if (Order[A].order(x, y) == GT) x else y)
    }

  /** The greatest value of `f(a)` for each element `a` of `fa`, or None if `fa` is empty. */
  def maximumOf[A, B: Order](fa: F[A])(f: A => B): Option[B] =
    foldLeft(fa, none[B]) {
      case (None, a) => some(f(a))
      case (Some(b), aa) => val bb = f(aa); some(if (Order[B].order(b, bb) == GT) b else bb)
    }

  /** The element `a` of `fa` which yields the greatest value of `f(a)`, or None if `fa` is empty. */
  def maximumBy[A, B: Order](fa: F[A])(f: A => B): Option[A] =
    foldLeft(fa, none[(A, B)]) {
      case (None, a) => some(a -> f(a))
      case (Some(x @ (a, b)), aa) => val bb = f(aa); some(if (Order[B].order(b, bb) == GT) x else aa -> bb)
    } map (_._1)

  /** The smallest element of `fa`, or None if `fa` is empty. */
  def minimum[A: Order](fa: F[A]): Option[A] =
    foldLeft(fa, none[A]) {
      case (None, y) => some(y)
      case (Some(x), y) => some(if (Order[A].order(x, y) == LT) x else y)
    }

  /** The smallest value of `f(a)` for each element `a` of `fa`, or None if `fa` is empty. */
  def minimumOf[A, B: Order](fa: F[A])(f: A => B): Option[B] =
    foldLeft(fa, none[B]) {
      case (None, a) => some(f(a))
      case (Some(b), aa) => val bb = f(aa); some(if (Order[B].order(b, bb) == LT) b else bb)
    }

  /** The element `a` of `fa` which yields the smallest value of `f(a)`, or None if `fa` is empty. */
  def minimumBy[A, B: Order](fa: F[A])(f: A => B): Option[A] =
    foldLeft(fa, none[(A, B)]) {
      case (None, a) => some(a -> f(a))
      case (Some(x @ (a, b)), aa) => val bb = f(aa); some(if (Order[B].order(b, bb) == LT) x else aa -> bb)
    } map (_._1)

  def sumr[A](fa: F[A])(implicit A: Monoid[A]): A =
    foldRight(fa, A.zero)(A.append)

  def sumr1Opt[A](fa: F[A])(implicit A: Semigroup[A]): Option[A] =
    foldRight1Opt(fa)(A.append(_, _))

  def suml[A](fa: F[A])(implicit A: Monoid[A]): A =
    foldLeft(fa, A.zero)(A.append(_, _))

  def suml1Opt[A](fa: F[A])(implicit A: Semigroup[A]): Option[A] =
    foldLeft1Opt(fa)(A.append(_, _))

  def msuml[G[_], A](fa: F[G[A]])(implicit G: PlusEmpty[G]): G[A] =
    foldLeft(fa, G.empty[A])(G.plus[A](_, _))

  def longDigits[A](fa: F[A])(implicit d: A <:< Digit): Long = foldLeft(fa, 0L)((n, a) => n * 10L + (a: Digit))
  /** Deforested alias for `toStream(fa).isEmpty`. */
  def empty[A](fa: F[A]): Boolean = all(fa)(_ => false)
  /** Whether `a` is an element of `fa`. */
  def element[A: Equal](fa: F[A], a: A): Boolean = any(fa)(Equal[A].equal(a, _))
  /** Insert an `A` between every A, yielding the sum. */
  def intercalate[A](fa: F[A], a: A)(implicit A: Monoid[A]): A =
    (foldRight(fa, none[A]) {(l, oa) =>
      some(A.append(l, oa map (A.append(a, _)) getOrElse A.zero))
    }).getOrElse(A.zero)

  /**
   * Splits the elements into groups that alternatively satisfy and don't satisfy the predicate p.
   */
  def splitWith[A](fa: F[A])(p: A => Boolean): List[NonEmptyList[A]] =
    foldRight(fa, (List[NonEmptyList[A]](), None : Option[Boolean]))((a, b) => {
      val pa = p(a)
      (b match {
        case (_, None) => NonEmptyList(a) :: Nil
        case (x, Some(q)) => if (pa == q) (a <:: x.head) :: x.tail else NonEmptyList(a) :: x
      }, Some(pa))
    })._1


  /**
   * Selects groups of elements that satisfy p and discards others.
   */
  def selectSplit[A](fa: F[A])(p: A => Boolean): List[NonEmptyList[A]] =
    foldRight(fa, (List[NonEmptyList[A]](), false))((a, xb) => xb match {
      case (x, b) => {
        val pa = p(a)
        (if (pa)
          if (b)
            (a <:: x.head) :: x.tail else
            NonEmptyList(a) :: x
        else x, pa)
      }
    })._1

  /** ``O(n log n)`` complexity */
  def distinct[A](fa: F[A])(implicit A: Order[A]): IList[A] =
    foldLeft(fa, (ISet.empty[A],IList.empty[A])) {
      case ((seen, acc), a) =>
        if (seen.notMember(a))
          (seen.insert(a), a :: acc)
        else (seen, acc)
    }._2.reverse

  /** ``O(n^2^)`` complexity */
  def distinctE[A](fa: F[A])(implicit A: Equal[A]): IList[A] =
    foldLeft(fa, IList.empty[A]) {
      case (seen, a) =>
        if (!IList.instances.element(seen,a))
          a :: seen
        else seen
    }.reverse

  def collapse[X[_], A](x: F[A])(implicit A: ApplicativePlus[X]): X[A] =
    foldRight(x, A.empty[A])((a, b) => A.plus(A.point(a), b))

  trait FoldableLaw {
    import std.vector._

    /** Left fold is consistent with foldMap. */
    def leftFMConsistent[A: Equal](fa: F[A]): Boolean =
      Equal[Vector[A]].equal(foldMap(fa)(Vector(_)),
                             foldLeft(fa, Vector.empty[A])(_ :+ _))

    /** Right fold is consistent with foldMap. */
    def rightFMConsistent[A: Equal](fa: F[A]): Boolean =
      Equal[Vector[A]].equal(foldMap(fa)(Vector(_)),
                             foldRight(fa, Vector.empty[A])(_ +: _))
  }
  def foldableLaw = new FoldableLaw {}

  ////
  val foldableSyntax: scalaz.syntax.FoldableSyntax[F] =
    new scalaz.syntax.FoldableSyntax[F] { def F = Foldable.this }
}

object Foldable {
  @inline def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F



  ////
  /**
   * Template trait to define `Foldable` in terms of `foldMap`.
   *
   * Example:
   * {{{
   * new Foldable[Option] with Foldable.FromFoldMap[Option] {
   *   def foldMap[A, B](fa: Option[A])(f: A => B)(implicit F: Monoid[B]) = fa match {
   *     case Some(a) => f(a)
   *     case None    => F.zero
   *   }
   * }
   * }}}
   */
  trait FromFoldMap[F[_]] extends Foldable[F] {
    override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B) =
      foldMap(fa)((a: A) => (Endo.endo(f(a, _: B)))) apply z
  }

  /**
   * Template trait to define `Foldable` in terms of `foldr`
   *
   * Example:
   * {{{
   * new Foldable[Option] with Foldable.FromFoldr[Option] {
   *   def foldr[A, B](fa: Option[A], z: B)(f: (A) => (=> B) => B) = fa match {
   *     case Some(a) => f(a)(z)
   *     case None => z
   *   }
   * }
   * }}}
   */
  trait FromFoldr[F[_]] extends Foldable[F] {
    override def foldMap[A, B](fa: F[A])(f: A => B)(implicit F: Monoid[B]) =
        foldr[A, B](fa, F.zero)( x => y => F.append(f(x),  y))
  }

  implicit def idInstance: Foldable[Id.Id] = Id.id

  ////
}
