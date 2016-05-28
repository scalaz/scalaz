package scalaz

////
/**
 * A [[scalaz.Foldable]] where `foldMap` is total over semigroups.
 * That is, `toList` cannot return an empty list.
 */
////
trait Foldable1[F[_]] extends Foldable[F] { self =>
  ////

  /**The product of Foldable1 `F` and `G`, `[x](F[x], G[x]])`, is a Foldable1 */
  def product[G[_]](implicit G0: Foldable1[G]): Foldable1[λ[α => (F[α], G[α])]] = 
    new ProductFoldable1[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /**The composition of Foldable1 `F` and `G`, `[x]F[G[x]]`, is a Foldable1 */
  def compose[G[_]: Foldable1]: Foldable1[λ[α => F[G[α]]]] =
    new CompositionFoldable1[F, G] {
      def F = self
      def G = implicitly
    }

  /** Map each element of the structure to a [[scalaz.Semigroup]], and combine the results. */
  def foldMap1[A,B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): B
  override def foldMap1Opt[A,B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): Option[B] = Some(foldMap1(fa)(f))

  /**Right-associative fold of a structure. */
  def foldMapRight1[A, B](fa: F[A])(z: A => B)(f: (A, => B) => B): B

  // derived functions
  override def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B =
    foldMap1(fa)(f)

  /**Right-associative fold of a structure. */
  def foldRight1[A](fa: F[A])(f: (A, => A) => A): A =
    foldMapRight1(fa)(identity)(f)

  override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B =
    foldMapRight1(fa)(f(_, z))(f)

  /**Left-associative fold of a structure. */
  def foldMapLeft1[A, B](fa: F[A])(z: A => B)(f: (B, A) => B): B = {
    import std.option._
    foldLeft(fa, none[B]) {
      case (None, r) => some(z(r))
      case (Some(l), r) => some(f(l, r))
    }.getOrElse(sys.error("foldMapLeft1"))
  }

  /**Left-associative fold of a structure. */
  def foldLeft1[A](fa: F[A])(f: (A, A) => A): A =
    foldMapLeft1(fa)(identity)(f)

  // XXX Would make a ⊥ with default foldMapLeft1; you can use it if
  // you also overrode foldMapLeft1
  // override def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B =
  //   foldMapLeft1(fa)(f(z, _))(f)

  /** Curried `foldRight1`. */
  final def foldr1[A](fa: F[A])(f: A => (=> A) => A): A = foldRight1(fa)((a, b) => f(a)(b))
  override def foldMapRight1Opt[A, B](fa: F[A])(z: A => B)(f: (A, => B) => B): Option[B] = Some(foldMapRight1(fa)(z)(f))
  override def foldr1Opt[A](fa: F[A])(f: A => (=> A) => A): Option[A] = Some(foldr1(fa)(f))
  /** Curried `foldLeft1`. */
  final def foldl1[A](fa: F[A])(f: A => A => A): A = foldLeft1(fa)((b, a) => f(b)(a))
  override def foldMapLeft1Opt[A, B](fa: F[A])(z: A => B)(f: (B, A) => B): Option[B] = Some(foldMapLeft1(fa)(z)(f))
  override def foldl1Opt[A](fa: F[A])(f: A => A => A): Option[A] = Some(foldl1(fa)(f))

  def fold1[M: Semigroup](t: F[M]): M = foldMap1[M, M](t)(identity)

  import Ordering.{GT, LT}

  /** The greatest element of `fa`. */
  def maximum1[A: Order](fa: F[A]): A =
    foldLeft1(fa)((x, y) => if (Order[A].order(x, y) == GT) x else y)

  /** The greatest value of `f(a)` for each element `a` of `fa`. */
  def maximumOf1[A, B: Order](fa: F[A])(f: A => B): B =
    Tag.unwrap(foldMap1(fa)(Tags.MaxVal.onF[A, B](f)))

  /** The element `a` of `fa` which yield the greatest value of `f(a)`. */
  def maximumBy1[A, B: Order](fa: F[A])(f: A => B): A =
    (maximumOf1(fa)(a => (a, f(a)))(Order.orderBy[(A, B), B](_._2)))._1

  /** The smallest element of `fa`. */
  def minimum1[A: Order](fa: F[A]): A =
    foldLeft1(fa)((x, y) => if (Order[A].order(x, y) == LT) x else y)

  /** The smallest value of `f(a)` for each element `a` of `fa`. */
  def minimumOf1[A, B: Order](fa: F[A])(f: A => B): B =
    Tag.unwrap(foldMap1(fa)(Tags.MinVal.onF[A, B](f)))

  /** The element `a` of `fa` which yield the smallest value of `f(a)`. */
  def minimumBy1[A, B: Order](fa: F[A])(f: A => B): A =
    (minimumOf1(fa)(a => (a, f(a)))(Order.orderBy[(A, B), B](_._2)))._1

  override def maximum[A: Order](fa: F[A]): Option[A] = Some(maximum1(fa))
  override def maximumOf[A, B: Order](fa: F[A])(f: A => B): Option[B] = Some(maximumOf1(fa)(f))
  override def maximumBy[A, B: Order](fa: F[A])(f: A => B): Option[A] = Some(maximumBy1(fa)(f))
  override def minimum[A: Order](fa: F[A]): Option[A] = Some(minimum1(fa))
  override def minimumOf[A, B: Order](fa: F[A])(f: A => B): Option[B] = Some(minimumOf1(fa)(f))
  override def minimumBy[A, B: Order](fa: F[A])(f: A => B): Option[A] = Some(minimumBy1(fa)(f))

  /** ``O(n log n)`` complexity */
  def distinct1[A](fa: F[A])(implicit A: Order[A]): NonEmptyList[A] =
    foldMapLeft1[A,(ISet[A],NonEmptyList[A])](fa)(a => (ISet.singleton(a), NonEmptyList(a))) {
      case ((seen, acc), a) =>
        if (seen.notMember(a))
          (seen.insert(a), a <:: acc)
        else (seen, acc)
    }._2.reverse

  /** ``O(n^2^)`` complexity */
  def distinctE1[A](fa: F[A])(implicit A: Equal[A]): NonEmptyList[A] =
    foldMapLeft1[A,NonEmptyList[A]](fa)(a => NonEmptyList(a)) {
      case (seen, a) =>
        if (!NonEmptyList.nonEmptyList.element(seen,a))
          a <:: seen
        else seen
    }.reverse

  def sumr1[A](fa: F[A])(implicit A: Semigroup[A]): A =
    foldRight1(fa)(A.append)

  def suml1[A](fa: F[A])(implicit A: Semigroup[A]): A =
    foldLeft1(fa)(A.append(_, _))

  def msuml1[G[_], A](fa: F[G[A]])(implicit G: Plus[G]): G[A] =
    foldLeft1[G[A]](fa)(G.plus[A](_, _))

  /** Insert an `A` between every A, yielding the sum. */
  def intercalate1[A](fa: F[A], a: A)(implicit A: Semigroup[A]): A =
    foldLeft1(fa)((x, y) => A.append(x, A.append(a, y)))

  override def intercalate[A: Monoid](fa: F[A], a: A): A =
    intercalate1(fa, a)

  def traverse1_[M[_], A, B](fa: F[A])(f: A => M[B])(implicit a: Apply[M], x: Semigroup[M[B]]): M[Unit] =
    a.map(foldMap1(fa)(f))(_ => ())

  def sequence1_[M[_], A](fa: F[M[A]])(implicit a: Apply[M], x: Semigroup[M[A]]): M[Unit] =
    traverse1_(fa)(x => x)

  /** always return `false` */
  final override def empty[A](fa: F[A]): Boolean = false

  /**The product of Foldable1 `F` and Foldable `G`, `[x](F[x], G[x]])`, is a Foldable1 */
  def product0[G[_]](implicit G0: Foldable[G]): Foldable1[λ[α => (F[α], G[α])]] =
    new ProductFoldable1L[F, G] {
      def F = self
      def G = G0
    }

  def toNel[A](fa: F[A]): NonEmptyList[A] =
    foldMapRight1(fa)(NonEmptyList.nel(_, IList.empty))(_ <:: _)

  def scanLeft1[A](fa: F[A])(f: (A, A) => A): NonEmptyList[A] =
    foldMapLeft1(fa)(NonEmptyList(_))((xs, x) => f(xs.head, x) <:: xs).reverse

  def scanRight1[A](fa: F[A])(f: (A, A) => A): NonEmptyList[A] =
    foldMapRight1(fa)(NonEmptyList(_))((x, xs) => f(x, xs.head) <:: xs)

  trait Foldable1Law extends FoldableLaw {
    import std.vector._

    /** Left fold is consistent with foldMap1. */
    def leftFM1Consistent[A: Equal](fa: F[A]): Boolean =
      Equal[Vector[A]].equal(foldMap1(fa)(Vector(_)),
                             foldMapLeft1(fa)(Vector(_))(_ :+ _))

    /** Right fold is consistent with foldMap1. */
    def rightFM1Consistent[A: Equal](fa: F[A]): Boolean =
      Equal[Vector[A]].equal(foldMap1(fa)(Vector(_)),
                             foldMapRight1(fa)(Vector(_))(_ +: _))
  }
  def foldable1Law = new Foldable1Law {}

  ////
  val foldable1Syntax = new scalaz.syntax.Foldable1Syntax[F] { def F = Foldable1.this }
}

object Foldable1 {
  @inline def apply[F[_]](implicit F: Foldable1[F]): Foldable1[F] = F

  ////

  ////
}
