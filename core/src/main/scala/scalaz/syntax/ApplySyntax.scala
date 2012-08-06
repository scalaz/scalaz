package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Apply` */
trait ApplyOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Apply[F]
  ////
  final def <*>[B](f: F[A => B]): F[B] = F.ap(self)(f)
  final def <**>[B, C](b: F[B])(f: (A, B) => C): F[C] = F.map2(self, b)(f)
  final def <***>[B, C, D](fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = F.map3(self, fb, fc)(f)
  final def <****>[B, C, D, E](fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = F.map4(self, fb, fc, fd)(f)
  final def <*****>[B, C, D, E, F0](fb: F[B], fc: F[C], fd: F[D], e: F[E])(f: (A, B, C, D, E) => F0): F[F0] = F.map5(self, fb, fc, fd, e)(f)

  /** Combine `self` and `fb` according to `Apply[F]` with a function that discards the `A`s */
  final def *>[B](fb: F[B]): F[B] = <**>(fb)((_, b) => b)

  /** Combine `self` and `fb` according to `Apply[F]` with a function that discards the `B`s */
  final def <*[B](fb: F[B]): F[A] = <**>(fb)((a, _) => a)

  /** Combine `self` and `fb` according to `Apply[F]` with a function that constructs a `Tuple2[A, B]` */
  final def <|*|>[B](fb: F[B]): F[(A, B)] = <**>(fb)((_, _))

  /**
   * DSL for constructing Applicative expressions.
   *
   * `(f1 |@| f2 |@| ... |@| fn)((v1, v2, ... vn) => ...)` is an alternative to `Apply[F].mapN(f1, f2, ..., fn)((v1, v2, ... vn) => ...)`
   *
   * `(f1 |@| f2 |@| ... |@| fn).tupled` is an alternative to `Apply[F].mapN(f1, f2, ..., fn)(TupleN.apply _)`
   *
   * Warning: each call to `|@|` leads to an allocation of wrapper object. For performance sensitive code, consider using
   *          [[scalaz.Apply]]`#mapN` directly.
   */
  final def |@|[B](fb: F[B]) = new ApplicativeBuilder[F, A, B] {
    val a: F[A] = self
    val b: F[B] = fb
  }
  /** Alias for `|@|` */
  final def ⊛[B](fb: F[B]) = |@|(fb)

  ////
}

trait ToApplyOps0 {
  implicit def ToApplyOpsUnapply[FA](v: FA)(implicit F0: Unapply[Apply, FA]) =
    new ApplyOps[F0.M,F0.A] { def self = F0(v); implicit def F: Apply[F0.M] = F0.TC }

}

trait ToApplyOps extends ToApplyOps0 with ToFunctorOps {
  implicit def ToApplyOps[F[_],A](v: F[A])(implicit F0: Apply[F]) =
    new ApplyOps[F,A] { def self = v; implicit def F: Apply[F] = F0 }

  ////

  ////
}

trait ApplySyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToApplyOps[A](v: F[A])(implicit F0: Apply[F]): ApplyOps[F, A] = new ApplyOps[F,A] { def self = v; implicit def F: Apply[F] = F0 }

  ////
  implicit def lift2[A,B,C](f: (A,B) => C)(implicit F: Apply[F]) = F.lift2(f)
  implicit def lift3[A,B,C,D](f: (A,B,C) => D)(implicit F: Apply[F]) = F.lift3(f)

  def ^[A,B,C](fa: => F[A], fb: => F[B])(
               f: (A, B) => C)(
               implicit F: Apply[F]): F[C] =
    F.map2(fa, fb)(f)

  def ^[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(
                 f: (A, B, C) => D)(
                 implicit F: Apply[F]): F[D] =
    F.map3(fa, fb, fc)(f)

  def ^[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(
                   f: (A,B,C,D) => E)(
                   implicit F: Apply[F]): F[E] =
    F.map4(fa, fb, fc, fd)(f)

  def ^[A,B,C,D,E,I](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(
                     f: (A,B,C,D,E) => I)(
                     implicit F: Apply[F]): F[I] =
    F.map5(fa, fb, fc, fd, fe)(f)

  def ^[A,B,C,D,E,I,J](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I])(
                       f: (A,B,C,D,E,I) => J)(
                       implicit F: Apply[F]): F[J] =
    F.map6(fa, fb, fc, fd, fe, fi)(f)

  ////
}
