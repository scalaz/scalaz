package scalaz
package syntax

import Tags.Parallel

/** @see [[scalaz.syntax.ApplyOps]]`#|@|` */
sealed trait ApplicativeBuilderOps[F[_]] {
  type ApplicativeBuilderType[_]

  def ⊛[X](fx: F[X]): ApplicativeBuilderType[X]
  final def |@|[X](fx: F[X]): ApplicativeBuilderType[X] = ⊛(fx)
}

sealed abstract class ApplicativeBuilder[F[_]] {
  type Function[_]
  type Tuple <: Product

  protected[this] val ap: Apply[F]
  protected[this] val toTuple: Function[Tuple]

  def apply[X](f: Function[X]): F[X]
  def tupled: F[Tuple] = apply(toTuple)

  def parApply[X](f: Function[X])(implicit ap: Apply.Par[F]): F[X]
  def parTupled(implicit ap: Apply.Par[F]): F[Tuple] = parApply(toTuple)
}

private[scalaz] final class ApplicativeBuilder2[F[_], A, B]
(fa: F[A], fb: F[B])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[C] = (A, B) => C
  type Tuple = (A, B)
  type ApplicativeBuilderType[C] = ApplicativeBuilder3[F, A, B, C]
  override protected[this] val toTuple: Function[Tuple] = Tuple2.apply
  override def apply[C](f: Function[C]): F[C] = ap.apply2(fa, fb)(f)
  override def parApply[C](f: Function[C])(implicit ap: Apply.Par[F]): F[C] =
    Tag.unwrap(ap.apply2(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb))(f))
  override def ⊛[C](fc: F[C]) = new ApplicativeBuilderType[C](fa, fb, fc)
}

private[scalaz] final class ApplicativeBuilder3[F[_], A, B, C]
(fa: F[A], fb: F[B], fc: F[C])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[D] = (A, B, C) => D
  type Tuple = (A, B, C)
  type ApplicativeBuilderType[D] = ApplicativeBuilder4[F, A, B, C, D]
  override protected[this] val toTuple: Function[Tuple] = Tuple3.apply
  override def apply[D](f: Function[D]): F[D] = ap.apply3(fa, fb, fc)(f)
  override def parApply[D](f: Function[D])(implicit ap: Apply.Par[F]): F[D] =
    Tag.unwrap(ap.apply3(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc))(f))
  override def ⊛[D](fd: F[D]) = new ApplicativeBuilderType[D](fa, fb, fc, fd)
}

private[scalaz] final class ApplicativeBuilder4[F[_], A, B, C, D]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[E] = (A, B, C, D) => E
  type Tuple = (A, B, C, D)
  type ApplicativeBuilderType[E] = ApplicativeBuilder5[F, A, B, C, D, E]
  override protected[this] val toTuple: Function[Tuple] = Tuple4.apply
  override def apply[E](f: Function[E]): F[E] = ap.apply4(fa, fb, fc, fd)(f)
  override def parApply[E](f: Function[E])(implicit ap: Apply.Par[F]): F[E] =
    Tag.unwrap(ap.apply4(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd))(f))
  override def ⊛[E](fe: F[E]) = new ApplicativeBuilderType[E](fa, fb, fc, fd, fe)
}

private[scalaz] final class ApplicativeBuilder5[F[_], A, B, C, D, E]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[G] = (A, B, C, D, E) => G
  type Tuple = (A, B, C, D, E)
  type ApplicativeBuilderType[G] = ApplicativeBuilder6[F, A, B, C, D, E, G]
  override protected[this] val toTuple: Function[Tuple] = Tuple5.apply
  override def apply[G](f: Function[G]): F[G] = ap.apply5(fa, fb, fc, fd, fe)(f)
  override def parApply[G](f: Function[G])(implicit ap: Apply.Par[F]): F[G] =
    Tag.unwrap(ap.apply5(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe))(f))
  override def ⊛[G](fg: F[G]) = new ApplicativeBuilderType[G](fa, fb, fc, fd, fe, fg)
}

private[scalaz] final class ApplicativeBuilder6[F[_], A, B, C, D, E, G]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[H] = (A, B, C, D, E, G) => H
  type Tuple = (A, B, C, D, E, G)
  type ApplicativeBuilderType[H] = ApplicativeBuilder7[F, A, B, C, D, E, G, H]
  override protected[this] val toTuple: Function[Tuple] = Tuple6.apply
  override def apply[H](f: Function[H]): F[H] = ap.apply6(fa, fb, fc, fd, fe, fg)(f)
  override def parApply[H](f: Function[H])(implicit ap: Apply.Par[F]): F[H] =
    Tag.unwrap(ap.apply6(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg))(f))
  override def ⊛[H](fh: F[H]) = new ApplicativeBuilderType[H](fa, fb, fc, fd, fe, fg, fh)
}

private[scalaz] final class ApplicativeBuilder7[F[_], A, B, C, D, E, G, H]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[I] = (A, B, C, D, E, G, H) => I
  type Tuple = (A, B, C, D, E, G, H)
  type ApplicativeBuilderType[I] = ApplicativeBuilder8[F, A, B, C, D, E, G, H, I]
  override protected[this] val toTuple: Function[Tuple] = Tuple7.apply
  override def apply[I](f: Function[I]): F[I] = ap.apply7(fa, fb, fc, fd, fe, fg, fh)(f)
  override def parApply[I](f: Function[I])(implicit ap: Apply.Par[F]): F[I] =
    Tag.unwrap(ap.apply7(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh))(f))
  override def ⊛[I](fi: F[I]) = new ApplicativeBuilderType[I](fa, fb, fc, fd, fe, fg, fh, fi)
}

private[scalaz] final class ApplicativeBuilder8[F[_], A, B, C, D, E, G, H, I]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[J] = (A, B, C, D, E, G, H, I) => J
  type Tuple = (A, B, C, D, E, G, H, I)
  type ApplicativeBuilderType[J] = ApplicativeBuilder9[F, A, B, C, D, E, G, H, I, J]
  override protected[this] val toTuple: Function[Tuple] = Tuple8.apply
  override def apply[J](f: Function[J]): F[J] = ap.apply8(fa, fb, fc, fd, fe, fg, fh, fi)(f)
  override def parApply[J](f: Function[J])(implicit ap: Apply.Par[F]): F[J] =
    Tag.unwrap(ap.apply8(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi))(f))
  override def ⊛[J](fj: F[J]) = new ApplicativeBuilderType[J](fa, fb, fc, fd, fe, fg, fh, fi, fj)
}

private[scalaz] final class ApplicativeBuilder9[F[_], A, B, C, D, E, G, H, I, J]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[K] = (A, B, C, D, E, G, H, I, J) => K
  type Tuple = (A, B, C, D, E, G, H, I, J)
  type ApplicativeBuilderType[K] = ApplicativeBuilder10[F, A, B, C, D, E, G, H, I, J, K]
  override protected[this] val toTuple: Function[Tuple] = Tuple9.apply
  override def apply[K](f: Function[K]): F[K] = ap.apply9(fa, fb, fc, fd, fe, fg, fh, fi, fj)(f)
  override def parApply[K](f: Function[K])(implicit ap: Apply.Par[F]): F[K] =
    Tag.unwrap(ap.apply9(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj))(f))
  override def ⊛[K](fk: F[K]) = new ApplicativeBuilderType[K](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk)
}

private[scalaz] final class ApplicativeBuilder10[F[_], A, B, C, D, E, G, H, I, J, K]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[L] = (A, B, C, D, E, G, H, I, J, K) => L
  type Tuple = (A, B, C, D, E, G, H, I, J, K)
  type ApplicativeBuilderType[L] = ApplicativeBuilder11[F, A, B, C, D, E, G, H, I, J, K, L]
  override protected[this] val toTuple: Function[Tuple] = Tuple10.apply
  override def apply[L](f: Function[L]): F[L] = ap.apply10(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk)(f)
  override def parApply[L](f: Function[L])(implicit ap: Apply.Par[F]): F[L] =
    Tag.unwrap(ap.apply10(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk))(f))
  override def ⊛[L](fl: F[L]) = new ApplicativeBuilderType[L](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl)
}

private[scalaz] final class ApplicativeBuilder11[F[_], A, B, C, D, E, G, H, I, J, K, L]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[M] = (A, B, C, D, E, G, H, I, J, K, L) => M
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L)
  type ApplicativeBuilderType[M] = ApplicativeBuilder12[F, A, B, C, D, E, G, H, I, J, K, L, M]
  override protected[this] val toTuple: Function[Tuple] = Tuple11.apply
  override def apply[M](f: Function[M]): F[M] = ap.apply11(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl)(f)
  override def parApply[M](f: Function[M])(implicit ap: Apply.Par[F]): F[M] =
    Tag.unwrap(ap.apply11(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl))(f))
  override def ⊛[M](fm: F[M]) = new ApplicativeBuilderType[M](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm)
}

private[scalaz] final class ApplicativeBuilder12[F[_], A, B, C, D, E, G, H, I, J, K, L, M]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M])
 (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[N] = (A, B, C, D, E, G, H, I, J, K, L, M) => N
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M)
  type ApplicativeBuilderType[N] = ApplicativeBuilder13[F, A, B, C, D, E, G, H, I, J, K, L, M, N]
  override protected[this] val toTuple: Function[Tuple] = Tuple12.apply
  override def apply[N](f: Function[N]): F[N] = ap.apply12(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm)(f)
  override def parApply[N](f: Function[N])(implicit ap: Apply.Par[F]): F[N] =
    Tag.unwrap(ap.apply12(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm))(f))
  override def ⊛[N](fn: F[N]) = new ApplicativeBuilderType[N](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn)
}

private[scalaz] final class ApplicativeBuilder13[F[_], A, B, C, D, E, G, H, I, J, K, L, M, N]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M], fn: F[N])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[O] = (A, B, C, D, E, G, H, I, J, K, L, M, N) => O
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M, N)
  type ApplicativeBuilderType[O] = ApplicativeBuilder14[F, A, B, C, D, E, G, H, I, J, K, L, M, N, O]
  override protected[this] val toTuple: Function[Tuple] = Tuple13.apply
  override def apply[O](f: Function[O]): F[O] = ap.apply13(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn)(f)
  override def parApply[O](f: Function[O])(implicit ap: Apply.Par[F]): F[O] =
    Tag.unwrap(ap.apply13(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm), Tag[F[N], Parallel](fn))(f))
  override def ⊛[O](fo: F[O]) = new ApplicativeBuilderType[O](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo)
}

private[scalaz] final class ApplicativeBuilder14[F[_], A, B, C, D, E, G, H, I, J, K, L, M, N, O]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M], fn: F[N], fo: F[O])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[P] = (A, B, C, D, E, G, H, I, J, K, L, M, N, O) => P
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M, N, O)
  type ApplicativeBuilderType[P] = ApplicativeBuilder15[F, A, B, C, D, E, G, H, I, J, K, L, M, N, O, P]
  override protected[this] val toTuple: Function[Tuple] = Tuple14.apply
  override def apply[P](f: Function[P]): F[P] = ap.apply14(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo)(f)
  override def parApply[P](f: Function[P])(implicit ap: Apply.Par[F]): F[P] =
    Tag.unwrap(ap.apply14(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm), Tag[F[N], Parallel](fn), Tag[F[O], Parallel](fo))(f))
  override def ⊛[P](fp: F[P]) = new ApplicativeBuilderType[P](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp)
}

private[scalaz] final class ApplicativeBuilder15[F[_], A, B, C, D, E, G, H, I, J, K, L, M, N, O, P]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M], fn: F[N], fo: F[O], fp: F[P])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[Q] = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P) => Q
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P)
  type ApplicativeBuilderType[Q] = ApplicativeBuilder16[F, A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q]
  override protected[this] val toTuple: Function[Tuple] = Tuple15.apply
  override def apply[Q](f: Function[Q]): F[Q] = ap.apply15(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp)(f)
  override def parApply[Q](f: Function[Q])(implicit ap: Apply.Par[F]): F[Q] =
    Tag.unwrap(ap.apply15(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm), Tag[F[N], Parallel](fn), Tag[F[O], Parallel](fo), Tag[F[P], Parallel](fp))(f))
  override def ⊛[Q](fq: F[Q]) = new ApplicativeBuilderType[Q](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq)
}

private[scalaz] final class ApplicativeBuilder16[F[_], A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M], fn: F[N], fo: F[O], fp: F[P], fq: F[Q])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[R] = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q) => R
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q)
  type ApplicativeBuilderType[R] = ApplicativeBuilder17[F, A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R]
  override protected[this] val toTuple: Function[Tuple] = Tuple16.apply
  override def apply[R](f: Function[R]): F[R] = ap.apply16(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq)(f)
  override def parApply[R](f: Function[R])(implicit ap: Apply.Par[F]): F[R] =
    Tag.unwrap(ap.apply16(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm), Tag[F[N], Parallel](fn), Tag[F[O], Parallel](fo), Tag[F[P], Parallel](fp), Tag[F[Q], Parallel](fq))(f))
  override def ⊛[R](fr: F[R]) = new ApplicativeBuilderType[R](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr)
}

private[scalaz] final class ApplicativeBuilder17[F[_], A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M], fn: F[N], fo: F[O], fp: F[P], fq: F[Q], fr: F[R])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[S] = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R) => S
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R)
  type ApplicativeBuilderType[S] = ApplicativeBuilder18[F, A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S]
  override protected[this] val toTuple: Function[Tuple] = Tuple17.apply
  override def apply[S](f: Function[S]): F[S] = ap.apply17(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr)(f)
  override def parApply[S](f: Function[S])(implicit ap: Apply.Par[F]): F[S] =
    Tag.unwrap(ap.apply17(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm), Tag[F[N], Parallel](fn), Tag[F[O], Parallel](fo), Tag[F[P], Parallel](fp), Tag[F[Q], Parallel](fq), Tag[F[R], Parallel](fr))(f))
  override def ⊛[S](fs: F[S]) = new ApplicativeBuilderType[S](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs)
}

private[scalaz] final class ApplicativeBuilder18[F[_], A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M], fn: F[N], fo: F[O], fp: F[P], fq: F[Q], fr: F[R], fs: F[S])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[T] = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S)
  type ApplicativeBuilderType[T] = ApplicativeBuilder19[F, A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
  override protected[this] val toTuple: Function[Tuple] = Tuple18.apply
  override def apply[T](f: Function[T]): F[T] = ap.apply18(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs)(f)
  override def parApply[T](f: Function[T])(implicit ap: Apply.Par[F]): F[T] =
    Tag.unwrap(ap.apply18(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm), Tag[F[N], Parallel](fn), Tag[F[O], Parallel](fo), Tag[F[P], Parallel](fp), Tag[F[Q], Parallel](fq), Tag[F[R], Parallel](fr), Tag[F[S], Parallel](fs))(f))
  override def ⊛[T](ft: F[T]) = new ApplicativeBuilderType[T](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft)
}

private[scalaz] final class ApplicativeBuilder19[F[_], A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M], fn: F[N], fo: F[O], fp: F[P], fq: F[Q], fr: F[R], fs: F[S], ft: F[T])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[U] = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)
  type ApplicativeBuilderType[U] = ApplicativeBuilder20[F, A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
  override protected[this] val toTuple: Function[Tuple] = Tuple19.apply
  override def apply[U](f: Function[U]): F[U] = ap.apply19(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft)(f)
  override def parApply[U](f: Function[U])(implicit ap: Apply.Par[F]): F[U] =
    Tag.unwrap(ap.apply19(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm), Tag[F[N], Parallel](fn), Tag[F[O], Parallel](fo), Tag[F[P], Parallel](fp), Tag[F[Q], Parallel](fq), Tag[F[R], Parallel](fr), Tag[F[S], Parallel](fs), Tag[F[T], Parallel](ft))(f))
  override def ⊛[U](fu: F[U]) = new ApplicativeBuilderType[U](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu)
}

private[scalaz] final class ApplicativeBuilder20[F[_], A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M], fn: F[N], fo: F[O], fp: F[P], fq: F[Q], fr: F[R], fs: F[S], ft: F[T], fu: F[U])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[V] = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)
  type ApplicativeBuilderType[V] = ApplicativeBuilder21[F, A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
  override protected[this] val toTuple: Function[Tuple] = Tuple20.apply
  override def apply[V](f: Function[V]): F[V] = ap.apply20(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu)(f)
  override def parApply[V](f: Function[V])(implicit ap: Apply.Par[F]): F[V] =
    Tag.unwrap(ap.apply20(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm), Tag[F[N], Parallel](fn), Tag[F[O], Parallel](fo), Tag[F[P], Parallel](fp), Tag[F[Q], Parallel](fq), Tag[F[R], Parallel](fr), Tag[F[S], Parallel](fs), Tag[F[T], Parallel](ft), Tag[F[U], Parallel](fu))(f))
  override def ⊛[V](fv: F[V]) = new ApplicativeBuilderType[V](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv)
}

private[scalaz] final class ApplicativeBuilder21[F[_], A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M], fn: F[N], fo: F[O], fp: F[P], fq: F[Q], fr: F[R], fs: F[S], ft: F[T], fu: F[U], fv: F[V])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[W] = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)
  type ApplicativeBuilderType[W] = ApplicativeBuilder22[F, A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]
  override protected[this] val toTuple: Function[Tuple] = Tuple21.apply
  override def apply[W](f: Function[W]): F[W] = ap.apply21(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv)(f)
  override def parApply[W](f: Function[W])(implicit ap: Apply.Par[F]): F[W] =
    Tag.unwrap(ap.apply21(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm), Tag[F[N], Parallel](fn), Tag[F[O], Parallel](fo), Tag[F[P], Parallel](fp), Tag[F[Q], Parallel](fq), Tag[F[R], Parallel](fr), Tag[F[S], Parallel](fs), Tag[F[T], Parallel](ft), Tag[F[U], Parallel](fu), Tag[F[V], Parallel](fv))(f))
  override def ⊛[W](fw: F[W]) = new ApplicativeBuilderType[W](fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv, fw)
}

private[scalaz] final class ApplicativeBuilder22[F[_], A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]
(fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M], fn: F[N], fo: F[O], fp: F[P], fq: F[Q], fr: F[R], fs: F[S], ft: F[T], fu: F[U], fv: F[V], fw: F[W])
(override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] {
  type Function[X] = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) => X
  type Tuple = (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W)
  override protected[this] val toTuple: Function[Tuple] = Tuple22.apply
  override def apply[X](f: Function[X]): F[X] = ap.apply22(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv, fw)(f)
  override def parApply[X](f: Function[X])(implicit ap: Apply.Par[F]): F[X] =
    Tag.unwrap(ap.apply22(Tag[F[A], Parallel](fa), Tag[F[B], Parallel](fb), Tag[F[C], Parallel](fc), Tag[F[D], Parallel](fd), Tag[F[E], Parallel](fe), Tag[F[G], Parallel](fg), Tag[F[H], Parallel](fh), Tag[F[I], Parallel](fi), Tag[F[J], Parallel](fj), Tag[F[K], Parallel](fk), Tag[F[L], Parallel](fl), Tag[F[M], Parallel](fm), Tag[F[N], Parallel](fn), Tag[F[O], Parallel](fo), Tag[F[P], Parallel](fp), Tag[F[Q], Parallel](fq), Tag[F[R], Parallel](fr), Tag[F[S], Parallel](fs), Tag[F[T], Parallel](ft), Tag[F[U], Parallel](fu), Tag[F[V], Parallel](fv), Tag[F[W], Parallel](fw))(f))
}
