package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Apply` */
sealed abstract class ApplyOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Apply[F]
  ////

  final def <*>[B](f: F[A => B]): F[B] = F.ap(self)(f)
  final def tuple[B](f: F[B]): F[(A,B)] = F.tuple2(self,f)

  /** Combine `self` and `fb` according to `Apply[F]` with a function that discards the `A`s */
  final def *>[B](fb: F[B]): F[B] = F.apply2(self,fb)((_,b) => b)

  /** Combine `self` and `fb` according to `Apply[F]` with a function that discards the `B`s */
  final def <*[B](fb: F[B]): F[A] = F.apply2(self,fb)((a,_) => a)

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

  implicit def lift2[F[_],A,B,C](f: (A,B) => C)(implicit F: Apply[F]) = F.lift2(f)
  implicit def lift3[F[_],A,B,C,D](f: (A,B,C) => D)(implicit F: Apply[F]) = F.lift3(f)
  implicit def lift4[F[_],A,B,C,D,E](f: (A,B,C,D) => E)(implicit F: Apply[F]) = F.lift4(f)

  def ^[F[_],A,B,C](fa: => F[A], fb: => F[B])(
               f: (A, B) => C)(implicit F: Apply[F]): F[C] =
    F.apply2(fa, fb)(f)

  def ^^[F[_],A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(
                 f: (A, B, C) => D)(implicit F: Apply[F]): F[D] =
    F.apply3(fa, fb, fc)(f)

  def ^^^[F[_],A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(
                   f: (A,B,C,D) => E)(implicit F: Apply[F]): F[E] =
    F.apply4(fa, fb, fc, fd)(f)

  def ^^^^[F[_],A,B,C,D,E,I](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(
                     f: (A,B,C,D,E) => I)(implicit F: Apply[F]): F[I] =
    F.apply5(fa, fb, fc, fd, fe)(f)

  def ^^^^^[F[_],A,B,C,D,E,I,J](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I])(
                       f: (A,B,C,D,E,I) => J)(implicit F: Apply[F]): F[J] =
    F.apply6(fa, fb, fc, fd, fe, fi)(f)

  def ^^^^^^[F[_],A,B,C,D,E,I,J,K](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J])(
                         f: (A,B,C,D,E,I,J) => K)(implicit F: Apply[F]): F[K] =
    F.apply7(fa, fb, fc, fd, fe, fi, fj)(f)

  ////
}

trait ApplySyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToApplyOps[A](v: F[A]): ApplyOps[F, A] = new ApplyOps[F,A] { def self = v; implicit def F: Apply[F] = ApplySyntax.this.F }

  def F: Apply[F]
  ////
  implicit def lift2[A,B,C](f: (A,B) => C) = F.lift2(f)
  implicit def lift3[A,B,C,D](f: (A,B,C) => D) = F.lift3(f)
  implicit def lift4[A,B,C,D,E](f: (A,B,C,D) => E) = F.lift4(f)

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
