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
  final def |@|[B](fb: F[B]): ApplicativeBuilder2[F, A, B] = new ApplicativeBuilder2[F, A, B](self, fb)

  /** Alias for `|@|` */
  final def ⊛[B](fb: F[B]): ApplicativeBuilder2[F, A, B] = |@|(fb)

  /**
   * Repeats this applicative action infinitely.
   */
  final def forever[B]: F[B] = F.forever(self)

  // Do not remove this comment; used as delimiter by `genTypeClasses` sbt task.
  ////
}

sealed trait ToApplyOpsU[TC[F[_]] <: Apply[F]] {
  implicit def ToApplyOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): ApplyOps[F0.M, F0.A] =
    new ApplyOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToApplyOps0[TC[F[_]] <: Apply[F]] extends ToApplyOpsU[TC] {
  implicit def ToApplyOps[F[_],A](v: F[A])(implicit F0: TC[F]): ApplyOps[F, A] =
    new ApplyOps[F, A](v)

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

  def ^^^^^^^[F[_],A,B,C,D,E,I,J,K,L](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K])(
                           f: (A,B,C,D,E,I,J,K) => L)(implicit F: TC[F]): F[L] =
    F.apply8(fa, fb, fc, fd, fe, fi, fj, fk)(f)

  def ^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L])(
                             f: (A,B,C,D,E,I,J,K,L) => M)(implicit F: TC[F]): F[M] =
    F.apply9(fa, fb, fc, fd, fe, fi, fj, fk, fl)(f)

  def ^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M])(
                               f: (A,B,C,D,E,I,J,K,L,M) => N)(implicit F: TC[F]): F[N] =
    F.apply10(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm)(f)

  def ^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N])(
                                 f: (A,B,C,D,E,I,J,K,L,M,N) => O)(implicit F: TC[F]): F[O] =
    F.apply11(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn)(f)

  def ^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O])(
                                   f: (A,B,C,D,E,I,J,K,L,M,N,O) => P)(implicit F: TC[F]): F[P] =
    F.apply12(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo)(f)

  def ^^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P,Q](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P])(
                                     f: (A,B,C,D,E,I,J,K,L,M,N,O,P) => Q)(implicit F: TC[F]): F[Q] =
    F.apply13(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp)(f)

  def ^^^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q])(
                                       f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q) => R)(implicit F: TC[F]): F[R] =
    F.apply14(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq)(f)

  def ^^^^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R])(
                                         f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R) => S)(implicit F: TC[F]): F[S] =
    F.apply15(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr)(f)

  def ^^^^^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S])(
                                           f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S) => T)(implicit F: TC[F]): F[T] =
    F.apply16(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs)(f)

  def ^^^^^^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T])(
                                             f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T) => U)(implicit F: TC[F]): F[U] =
    F.apply17(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft)(f)

  def ^^^^^^^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U])(
                                               f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U) => V)(implicit F: TC[F]): F[V] =
    F.apply18(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu)(f)

  def ^^^^^^^^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U], fv: => F[V])(
                                                 f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => W)(implicit F: TC[F]): F[W] =
    F.apply19(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv)(f)

  def ^^^^^^^^^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U], fv: => F[V], fw: => F[W])(
                                                   f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) => X)(implicit F: TC[F]): F[X] =
    F.apply20(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv, fw)(f)

  def ^^^^^^^^^^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U], fv: => F[V], fw: => F[W], fx: => F[X])(
                                                     f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) => Y)(implicit F: TC[F]): F[Y] =
    F.apply21(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv, fw, fx)(f)

  def ^^^^^^^^^^^^^^^^^^^^^[F[_],A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U], fv: => F[V], fw: => F[W], fx: => F[X], fy: => F[Y])(
                                                       f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) => Z)(implicit F: TC[F]): F[Z] =
    F.apply22(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv, fw, fx, fy)(f)

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

  def ^^^^^^^[A,B,C,D,E,I,J,K,L](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K])(
                           f: (A,B,C,D,E,I,J,K) => L): F[L] =
    F.apply8(fa, fb, fc, fd, fe, fi, fj, fk)(f)

  def ^^^^^^^^[A,B,C,D,E,I,J,K,L,M](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L])(
                             f: (A,B,C,D,E,I,J,K,L) => M): F[M] =
    F.apply9(fa, fb, fc, fd, fe, fi, fj, fk, fl)(f)

  def ^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M])(
                               f: (A,B,C,D,E,I,J,K,L,M) => N): F[N] =
    F.apply10(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm)(f)

  def ^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N])(
                                 f: (A,B,C,D,E,I,J,K,L,M,N) => O): F[O] =
    F.apply11(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn)(f)

  def ^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O])(
                                   f: (A,B,C,D,E,I,J,K,L,M,N,O) => P): F[P] =
    F.apply12(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo)(f)

  def ^^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P,Q](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P])(
                                     f: (A,B,C,D,E,I,J,K,L,M,N,O,P) => Q): F[Q] =
    F.apply13(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp)(f)

  def ^^^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q])(
                                       f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q) => R): F[R] =
    F.apply14(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq)(f)

  def ^^^^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R])(
                                         f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R) => S): F[S] =
    F.apply15(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr)(f)

  def ^^^^^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S])(
                                           f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S) => T): F[T] =
    F.apply16(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs)(f)

  def ^^^^^^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T])(
                                             f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T) => U): F[U] =
    F.apply17(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft)(f)

  def ^^^^^^^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U])(
                                               f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U) => V): F[V] =
    F.apply18(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu)(f)

  def ^^^^^^^^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U], fv: => F[V])(
                                                 f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V) => W): F[W] =
    F.apply19(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv)(f)

  def ^^^^^^^^^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U], fv: => F[V], fw: => F[W])(
                                                   f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W) => X): F[X] =
    F.apply20(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv, fw)(f)

  def ^^^^^^^^^^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U], fv: => F[V], fw: => F[W], fx: => F[X])(
                                                     f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X) => Y): F[Y] =
    F.apply21(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv, fw, fx)(f)

  def ^^^^^^^^^^^^^^^^^^^^^[A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U], fv: => F[V], fw: => F[W], fx: => F[X], fy: => F[Y])(
                                                       f: (A,B,C,D,E,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y) => Z): F[Z] =
    F.apply22(fa, fb, fc, fd, fe, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv, fw, fx, fy)(f)

  ////
}
