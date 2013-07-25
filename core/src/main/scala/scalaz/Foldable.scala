package scalaz

////
/**
 * A type parameter implying the ability to extract zero or more
 * values of that type.
 */
////
trait Foldable[F[_]]  { self =>
  ////
  import collection.generic.CanBuildFrom
  import collection.immutable.IndexedSeq

  /** Map each element of the structure to a [[scalaz.Monoid]], and combine the results. */
  def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B
  /** As `foldMap` but returning `None` if the foldable is empty and `Some` otherwise */
  def foldMap1Opt[A,B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): Option[B] = {
    import syntax.std.option._, std.option._
    foldMap(fa)(x => some(f(x)))
  }

  /**Right-associative fold of a structure. */
  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

  /**The composition of Foldables `F` and `G`, `[x]F[G[x]]`, is a Foldable */
  def compose[G[_]](implicit G0: Foldable[G]): Foldable[({type λ[α] = F[G[α]]})#λ] = new CompositionFoldable[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Foldables `F` and `G`, `[x](F[x], G[x]])`, is a Foldable */
  def product[G[_]](implicit G0: Foldable[G]): Foldable[({type λ[α] = (F[α], G[α])})#λ] = new ProductFoldable[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**Left-associative fold of a structure. */
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B = {
    import Dual._, Endo._, syntax.std.all._
    foldMap(fa)((a: A) => Dual(Endo.endo(f.flip.curried(a))))(dualMonoid) apply (z)
  }

  /**Right-associative, monadic fold of a structure. */
  def foldRightM[G[_], A, B](fa: F[A], z: => B)(f: (A, => B) => G[B])(implicit M: Monad[G]): G[B] =
    foldLeft[A, B => G[B]](fa, M.point(_))((b, a) => w => M.bind(f(a, w))(b))(z)

  /**Left-associative, monadic fold of a structure. */
  def foldLeftM[G[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit M: Monad[G]): G[B] =
    foldRight[A, B => G[B]](fa, M.point(_))((a, b) => w => M.bind(f(w, a))(b))(z)
  
  /** Combine the elements of a structure using a monoid. */
  def fold[M: Monoid](t: F[M]): M = foldMap[M, M](t)(x => x)

  /** Strict traversal in an applicative functor `M` that ignores the result of `f`. */  
  def traverse_[M[_], A, B](fa: F[A])(f: A => M[B])(implicit a: Applicative[M]): M[Unit] =
    foldLeft(fa, a.pure(()))((x, y) => a.ap(f(y))(a.map(x)(_ => _ => ())))

  /** `traverse_` specialized to `State` **/
  def traverseS_[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, Unit] =
    traverse_[({type λ[α]=State[S, α]})#λ, A, B](fa)(f)

  /** Strict sequencing in an applicative functor `M` that ignores the value in `fa`. */
  def sequence_[M[_], A](fa: F[M[A]])(implicit a: Applicative[M]): M[Unit] =
    traverse_(fa)(x => x)

  /** `sequence_` specialized to `State` **/
  def sequenceS_[S, A](fga: F[State[S, A]]): State[S, Unit] =
    traverseS_(fga)(x => x)

  /**Curried version of `foldRight` */
  final def foldr[A, B](fa: F[A], z: => B)(f: A => (=> B) => B): B = foldRight(fa, z)((a, b) => f(a)(b))
  def foldRight1Opt[A](fa: F[A])(f: (A, => A) => A): Option[A] = foldRight(fa, None: Option[A])((a, optA) => optA map (aa => f(a, aa)) orElse Some(a))
  def foldr1Opt[A](fa: F[A])(f: A => (=> A) => A): Option[A] = foldr(fa, None: Option[A])(a => optA => optA map (aa => f(a)(aa)) orElse Some(a))

  /**Curried version of `foldLeft` */
  final def foldl[A, B](fa: F[A], z: B)(f: B => A => B) = foldLeft(fa, z)((b, a) => f(b)(a))
  def foldLeft1Opt[A](fa: F[A])(f: (A, A) => A): Option[A] = foldLeft(fa, None: Option[A])((optA, a) => optA map (aa => f(aa, a)) orElse Some(a))
  def foldl1Opt[A](fa: F[A])(f: A => A => A): Option[A] = foldl(fa, None: Option[A])(optA => a => optA map (aa => f(aa)(a)) orElse Some(a))

  /**Curried version of `foldRightM` */
  final def foldrM[G[_], A, B](fa: F[A], z: => B)(f: A => ( => B) => G[B])(implicit M: Monad[G]): G[B] = 
    foldRightM(fa, z)((a, b) => f(a)(b))

  /**Curried version of `foldLeftM` */
  final def foldlM[G[_], A, B](fa: F[A], z: => B)(f: B => A => G[B])(implicit M: Monad[G]): G[B] =
    foldLeftM(fa, z)((b, a) => f(b)(a))

  /** Deforested alias for `toStream(fa).size`. */
  def count[A](fa: F[A]): Int = foldLeft(fa, 0)((b, _) => b + 1)

  /** Alias for `count`. */
  def length[A](fa: F[A]): Int = count(fa)

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

  /** Unbiased sum of monoidal values. */
  @deprecated("use `fold`, it has the exact same signature and implementation", "7.1")
  def foldMapIdentity[A](fa: F[A])(implicit F: Monoid[A]): A = foldMap(fa)(a => a)
  def toList[A](fa: F[A]): List[A] = foldLeft(fa, scala.List[A]())((t, h) => h :: t).reverse
  def toIndexedSeq[A](fa: F[A]): IndexedSeq[A] = foldLeft(fa, IndexedSeq[A]())(_ :+ _)
  def toSet[A](fa: F[A]): Set[A] = foldLeft(fa, Set[A]())(_ + _)
  def toStream[A](fa: F[A]): Stream[A] = foldRight[A, Stream[A]](fa, Stream.empty)(Stream.cons(_, _))
  def to[A, G[_]](fa: F[A])(implicit c: CanBuildFrom[Nothing, A, G[A]]): G[A] =
    foldLeft(fa, c())(_ += _).result

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

  def longDigits[A](fa: F[A])(implicit d: A <:< Digit): Long = foldLeft(fa, 0L)((n, a) => n * 10L + (a: Digit))
  /** Deforested alias for `toStream(fa).isEmpty`. */
  def empty[A](fa: F[A]): Boolean = all(fa)(_ => false)
  /** Whether `a` is an element of `fa`. */
  def element[A: Equal](fa: F[A], a: A): Boolean = any(fa)(Equal[A].equal(a, _))
  /** Insert an `A` between every A, yielding the sum. */
  def intercalate[A](fa: F[A], a: A)(implicit A: Monoid[A]): A =
    (foldRight(fa, none[A]) {
      case (l, None) => some(l)
      case (l, Some(r)) => some(A.append(l, A.append(a, r)))
    }).getOrElse(A.zero)

  /**
   * Splits the elements into groups that alternatively satisfy and don't satisfy the predicate p.
   */
  def splitWith[A](fa: F[A])(p: A => Boolean): List[List[A]] =
    foldRight(fa, (List[List[A]](), None : Option[Boolean]))((a, b) => {
      val pa = p(a)
      (b match {
        case (_, None) => List(List(a))
        case (x, Some(q)) => if (pa == q) (a :: x.head) :: x.tail else List(a) :: x
      }, Some(pa))
    })._1


  /**
   * Selects groups of elements that satisfy p and discards others.
   */
  def selectSplit[A](fa: F[A])(p: A => Boolean): List[List[A]] =
    foldRight(fa, (List[List[A]](), false))((a, xb) => xb match {
      case (x, b) => {
        val pa = p(a)
        (if (pa)
          if (b)
            (a :: x.head) :: x.tail else
            List(a) :: x
        else x, pa)
      }
    })._1

  def collapse[X[_], A](x: F[A])(implicit F: Foldable[F], A: ApplicativePlus[X]): X[A] =
    F.foldRight(x, A.empty[A])((a, b) => A.plus(A.point(a), b))

  def collapse2[G[_], X[_], A](x: F[G[A]])(implicit
                                            F: Foldable[F]
                                          , G: Foldable[G]
                                          , A: ApplicativePlus[X]): X[A] = {
    implicit val Z = F compose G
    Z collapse x
  }

  def collapse3[G[_], H[_], X[_], A](x: F[G[H[A]]])(implicit
                                                     F: Foldable[F]
                                                   , G: Foldable[G]
                                                   , H: Foldable[H]
                                                   , A: ApplicativePlus[X]): X[A] = {
    implicit val Z = F compose G compose H
    Z.collapse(x)
  }

  def collapse4[G[_], H[_], I[_], X[_], A](x: F[G[H[I[A]]]])(implicit
                                                              F: Foldable[F]
                                                            , G: Foldable[G]
                                                            , H: Foldable[H]
                                                            , I: Foldable[I]
                                                            , A: ApplicativePlus[X]): X[A] = {
    implicit val Z = F compose G compose H compose I
    Z.collapse(x)
  }

  def collapse5[G[_], H[_], I[_], J[_], X[_], A](x: F[G[H[I[J[A]]]]])(implicit
                                                                       F: Foldable[F]
                                                                     , G: Foldable[G]
                                                                     , H: Foldable[H]
                                                                     , I: Foldable[I]
                                                                     , J: Foldable[J]
                                                                     , A: ApplicativePlus[X]): X[A] = {
    implicit val Z = F compose G compose H compose I compose J
    Z.collapse(x)
  }

  def collapse6[G[_], H[_], I[_], J[_], K[_], X[_], A](x: F[G[H[I[J[K[A]]]]]])(implicit
                                                                                F: Foldable[F]
                                                                              , G: Foldable[G]
                                                                              , H: Foldable[H]
                                                                              , I: Foldable[I]
                                                                              , J: Foldable[J]
                                                                              , K: Foldable[K]
                                                                              , A: ApplicativePlus[X]): X[A] = {
    implicit val Z = F compose G compose H compose I compose J compose K
    Z.collapse(x)
  }

  def collapse7[G[_], H[_], I[_], J[_], K[_], L[_], X[_], A](x: F[G[H[I[J[K[L[A]]]]]]])(implicit
                                                                                         F: Foldable[F]
                                                                                       , G: Foldable[G]
                                                                                       , H: Foldable[H]
                                                                                       , I: Foldable[I]
                                                                                       , J: Foldable[J]
                                                                                       , K: Foldable[K]
                                                                                       , L: Foldable[L]
                                                                                       , A: ApplicativePlus[X]): X[A] = {
    implicit val Z = F compose G compose H compose I compose J compose K compose L
    Z.collapse(x)
  }

  ////
  val foldableSyntax = new scalaz.syntax.FoldableSyntax[F] { def F = Foldable.this }
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

  ////
}
