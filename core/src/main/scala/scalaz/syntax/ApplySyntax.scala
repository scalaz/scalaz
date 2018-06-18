package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Apply` */
final class ApplyOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Apply[F]) extends Ops[F[A]] {
  ////

  final def <*>[B](f: F[A => B]): F[B] = F.ap(self)(f)
  final def tuple[B](f: F[B]): F[(A,B)] = F.tuple2(self,f)

  /** Combine `self` and `fb` according to `Apply[F]` and discard the `A`(s) */
  final def *>[B](fb: F[B]): F[B] = F.discardLeft(self,fb)

  /** Combine `self` and `fb` according to `Apply[F]` and discard the `A`(s), except 'fb' is non-strictly evaluated. */
  final def `*>ByName`[B](fb: => F[B]): F[B] = F.discardLeft(self,fb)

  /** Combine `self` and `fb` according to `Apply[F]` and discard the `B`(s) */
  final def <*[B](fb: F[B]): F[A] = F.discardRight(self,fb)

  /** Combine `self` and `fb` according to `Apply[F]` and discard the `B`(s), except 'fb' is non-strictly evaluated. */
  final def `<*ByName`[B](fb: => F[B]): F[A] = F.discardRight(self,fb)

  /**
   * DSL for constructing Applicative expressions.
   *
   * `(f1 |@| f2 |@| ... |@| fn)((v1, v2, ... vn) => ...)` is an alternative to `Apply[F].applyN(f1, f2, ..., fn)((v1, v2, ... vn) => ...)`
   *
   * `(f1 |@| f2 |@| ... |@| fn).tupled` is an alternative to `Apply[F].applyN(f1, f2, ..., fn)(TupleN.apply _)`
   *
   * Warning: each call to `|@|` leads to an allocation of wrapper object. For performance sensitive code, consider using
   *          [[scalaz.Apply]]`#applyN` directly.
   */
  final def |@|[B](fb: F[B]) = new ApplicativeBuilder[F, A, B](self, fb)

  /** Alias for `|@|` */
  final def ⊛[B](fb: F[B]): ApplicativeBuilder[F, A, B] = |@|(fb)

  /**
   * Repeats this applicative action infinitely.
   */
  final def forever[B]: F[B] = F.forever(self)

  // Do not remove this comment; used as delimiter by `genTypeClasses` sbt task.
  ////
}

sealed trait ToApplyOpsU[TC[F[_]] <: Apply[F]] {
  implicit def ToApplyOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new ApplyOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToApplyOps0[TC[F[_]] <: Apply[F]] extends ToApplyOpsU[TC] {
  implicit def ToApplyOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new ApplyOps[F,A](v)

  ////

  def ^[F[_],A,B,C](fa: => F[A], fb: => F[B])(
               f: (A, B) => C)(implicit F: TC[F]): F[C] =
    F.apply2(fa, fb)(f)

  def ^^[F[_],A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(
                 f: (A, B, C) => D)(implicit F: TC[F]): F[D] =
    F.apply3(fa, fb, fc)(f)

  def ^^^[F[_],A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(
                   f: (A,B,C,D) => E)(implicit F: TC[F]): F[E] =
    F.apply4(fa, fb, fc, fd)(f)

  def ^^^^[F[_],A,B,C,D,E,I](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(
                     f: (A,B,C,D,E) => I)(implicit F: TC[F]): F[I] =
    F.apply5(fa, fb, fc, fd, fe)(f)

  def ^^^^^[F[_],A,B,C,D,E,I,J](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I])(
                       f: (A,B,C,D,E,I) => J)(implicit F: TC[F]): F[J] =
    F.apply6(fa, fb, fc, fd, fe, fi)(f)

  def ^^^^^^[F[_],A,B,C,D,E,I,J,K](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J])(
                         f: (A,B,C,D,E,I,J) => K)(implicit F: TC[F]): F[K] =
    F.apply7(fa, fb, fc, fd, fe, fi, fj)(f)

  ////
}

trait ToApplyOps[TC[F[_]] <: Apply[F]] extends ToApplyOps0[TC] with ToFunctorOps[TC]

trait ApplySyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToApplyOps[A](v: F[A]): ApplyOps[F, A] = new ApplyOps[F,A](v)(ApplySyntax.this.F)

  def F: Apply[F]
  ////
  def ^[A,B,C](fa: => F[A], fb: => F[B])(
               f: (A, B) => C): F[C] =
    F.apply2(fa, fb)(f)

  def ^^[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(
                 f: (A, B, C) => D): F[D] =
    F.apply3(fa, fb, fc)(f)

  def ^^^[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(
                   f: (A,B,C,D) => E): F[E] =
    F.apply4(fa, fb, fc, fd)(f)

  def ^^^^[A,B,C,D,E,I](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(
                     f: (A,B,C,D,E) => I): F[I] =
    F.apply5(fa, fb, fc, fd, fe)(f)

  def ^^^^^[A,B,C,D,E,I,J](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I])(
                       f: (A,B,C,D,E,I) => J): F[J] =
    F.apply6(fa, fb, fc, fd, fe, fi)(f)

  def ^^^^^^[A,B,C,D,E,I,J,K](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J])(
                         f: (A,B,C,D,E,I,J) => K): F[K] =
    F.apply7(fa, fb, fc, fd, fe, fi, fj)(f)

  ////
}
