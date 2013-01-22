package scalaz

////
/**
 * A [[scalaz.Foldable]] where `foldMap` is total over semigroups.
 * That is, `toList` cannot return an empty list.
 */
////
trait Foldable1[F[_]] extends Foldable[F] { self =>
  ////
  /** Map each element of the structure to a [[scalaz.Semigroup]], and combine the results. */
  def foldMap1[A,B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): B

  /**Right-associative fold of a structure. */
  def foldRight1[A](fa: F[A])(f: (A, => A) => A): A

  // derived functions
  override def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B =
    foldMap1(fa)(f)

  /**Left-associative fold of a structure. */
  def foldLeft1[A](fa: F[A])(f: (A, A) => A): A = {
    import std.option._
    foldLeft(fa, none[A]) {
      case (None, r) => some(r)
      case (Some(l), r) => some(f(l, r))
    }.getOrElse(sys.error("foldLeft1"))
  }

  /** Curried `foldRight1`. */
  final def foldr1[A](fa: F[A])(f: A => (=> A) => A): A = foldRight1(fa)((a, b) => f(a)(b))
  /** Curried `foldLeft1`. */
  final def foldl1[A](fa: F[A])(f: A => A => A): A = foldLeft1(fa)((b, a) => f(b)(a))

  def fold1[M: Semigroup](t: F[M]): M = foldMap1[M, M](t)(identity)

  import Ordering.{GT, LT}
  /** The greatest element of `fa`. */
  def maximum1[A: Order](fa: F[A]): A = foldl1(fa)(x => y => if (Order[A].order(x, y) == GT) x else y)
  /** The smallest element of `fa`. */
  def minimum1[A: Order](fa: F[A]): A = foldl1(fa)(x => y => if (Order[A].order(x, y) == LT) x else y)

  def traverse1_[M[_], A, B](fa: F[A])(f: A => M[B])(implicit a: Apply[M], x: Semigroup[M[B]]): M[Unit] =
    a.map(foldMap1(fa)(f))(_ => ())

  def sequence1_[M[_], A, B](fa: F[M[A]])(implicit a: Apply[M], x: Semigroup[M[A]]): M[Unit] =
    traverse1_(fa)(x => x)

  ////
  val foldable1Syntax = new scalaz.syntax.Foldable1Syntax[F] { def F = Foldable1.this }
}

object Foldable1 {
  @inline def apply[F[_]](implicit F: Foldable1[F]): Foldable1[F] = F

  ////

  ////
}
