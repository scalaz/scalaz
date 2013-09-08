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
  def product[G[_]](implicit G0: Foldable1[G]): Foldable1[({type λ[α] = (F[α], G[α])})#λ] = new ProductFoldable1[F, G] {
    implicit def F = self

    implicit def G = G0
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
    foldMapRight1(fa)(conforms)(f)

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
    foldMapLeft1(fa)(conforms)(f)

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
    foldl1(fa)(x => y => if (Order[A].order(x, y) == GT) x else y)

  /** The greatest value of `f(a)` for each element `a` of `fa`. */
  def maximumOf1[A, B: Order](fa: F[A])(f: A => B): B =
    foldMap1(fa)(f andThen Tags.MaxVal)

  /** The element `a` of `fa` which yield the greatest value of `f(a)`. */
  def maximumBy1[A, B: Order](fa: F[A])(f: A => B): A =
    (maximumOf1(fa)(a => (a, f(a)))(Order.orderBy[(A, B), B](_._2)))._1

  /** The smallest element of `fa`. */
  def minimum1[A: Order](fa: F[A]): A =
    foldl1(fa)(x => y => if (Order[A].order(x, y) == LT) x else y)

  /** The smallest value of `f(a)` for each element `a` of `fa`. */
  def minimumOf1[A, B: Order](fa: F[A])(f: A => B): B =
    foldMap1(fa)(f andThen Tags.MinVal)

  /** The element `a` of `fa` which yield the smallest value of `f(a)`. */
  def minimumBy1[A, B: Order](fa: F[A])(f: A => B): A =
    (minimumOf1(fa)(a => (a, f(a)))(Order.orderBy[(A, B), B](_._2)))._1

  def traverse1_[M[_], A, B](fa: F[A])(f: A => M[B])(implicit a: Apply[M], x: Semigroup[M[B]]): M[Unit] =
    a.map(foldMap1(fa)(f))(_ => ())

  def sequence1_[M[_], A, B](fa: F[M[A]])(implicit a: Apply[M], x: Semigroup[M[A]]): M[Unit] =
    traverse1_(fa)(x => x)

  /** always return `false` */
  final override def empty[A](fa: F[A]): Boolean = false

  /**The product of Foldable1 `F` and Foldable `G`, `[x](F[x], G[x]])`, is a Foldable1 */
  def product0[G[_]](implicit G0: Foldable[G]): Foldable1[({type λ[α] = (F[α], G[α])})#λ] =
    new ProductFoldable1L[F, G] {
      def F = self
      def G = G0
    }

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
