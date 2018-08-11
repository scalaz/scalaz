package scalaz
package syntax

/** @see [[scalaz.syntax.ApplyOps]]`#|@|` */
sealed trait ApplicativeBuilderOps[F[_]] {
  type ApplicativeBuilderType[_]

  def ⊛[X](fx: F[X]): ApplicativeBuilderType[X]
  def |@|[X](fx: F[X]): ApplicativeBuilderType[X] = ⊛(fx)
}

sealed abstract class ApplicativeBuilder[F[_]] {
  type Function[_]
  type Tuple <: Product

  protected[this] val ap: Apply[F]
  protected[this] val toTuple: Function[Tuple]

  def apply[X](f: Function[X]): F[X]
  def tupled: F[Tuple] = apply(toTuple)
}

private[scalaz] final class ApplicativeBuilder2[F[_], A, B](fa: F[A], fb: F[B])
                                                           (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[C] = (A, B) => C
  type Tuple = (A, B)
  type ApplicativeBuilderType[C] = ApplicativeBuilder3[F, A, B, C]

  override protected[this] val toTuple: Function[Tuple] = Tuple2.apply

  override def apply[C](f: Function[C]): F[C] = ap.apply2(fa, fb)(f)
  override def ⊛[C](fc: F[C]) = new ApplicativeBuilderType[C](fa, fb, fc)
}


private[scalaz] final class ApplicativeBuilder3[F[_], A, B, C](fa: F[A], fb: F[B], fc: F[C])
                                                              (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[D] = (A, B, C) => D
  type Tuple = (A, B, C)
  type ApplicativeBuilderType[D] = ApplicativeBuilder4[F, A, B, C, D]

  override protected[this] val toTuple: Function[Tuple] = Tuple3.apply

  override def apply[D](f: Function[D]): F[D] = ap.apply3(fa, fb, fc)(f)
  override def ⊛[D](fd: F[D]) = new ApplicativeBuilderType[D](fa, fb, fc, fd)
}


private[scalaz] final class ApplicativeBuilder4[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C], fd: F[D])
                                                                 (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[E] = (A, B, C, D) => E
  type Tuple = (A, B, C, D)
  type ApplicativeBuilderType[E] = ApplicativeBuilder5[F, A, B, C, D, E]

  override protected[this] val toTuple: Function[Tuple] = Tuple4.apply

  override def apply[E](f: Function[E]): F[E] = ap.apply4(fa, fb, fc, fd)(f)
  override def ⊛[E](fe: F[E]) = new ApplicativeBuilderType[E](fa, fb, fc, fd, fe)
}


private[scalaz] final class ApplicativeBuilder5[F[_], A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])
                                                                    (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[FF] = (A, B, C, D, E) => FF
  type Tuple = (A, B, C, D, E)
  type ApplicativeBuilderType[FF] = ApplicativeBuilder6[F, A, B, C, D, E, FF]

  override protected[this] val toTuple: Function[Tuple] = Tuple5.apply

  override def apply[FF](f: Function[FF]): F[FF] = ap.apply5(fa, fb, fc, fd, fe)(f)
  override def ⊛[FF](ff: F[FF]) = new ApplicativeBuilderType[FF](fa, fb, fc, fd, fe, ff)
}


private[scalaz] final class ApplicativeBuilder6[F[_], A, B, C, D, E, FF](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF])
                                                                        (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[G] = (A, B, C, D, E, FF) => G
  type Tuple = (A, B, C, D, E, FF)
  type ApplicativeBuilderType[G] = ApplicativeBuilder7[F, A, B, C, D, E, FF, G]

  override protected[this] val toTuple: Function[Tuple] = Tuple6.apply

  override def apply[G](f: Function[G]): F[G] = ap.apply6(fa, fb, fc, fd, fe, ff)(f)
  override def ⊛[G](fg: F[G]) = new ApplicativeBuilderType[G](fa, fb, fc, fd, fe, ff, fg)
}


private[scalaz] final class ApplicativeBuilder7[F[_], A, B, C, D, E, FF, G](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G])
                                                                           (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[H] = (A, B, C, D, E, FF, G) => H
  type Tuple = (A, B, C, D, E, FF, G)
  type ApplicativeBuilderType[H] = ApplicativeBuilder8[F, A, B, C, D, E, FF, G, H]

  override protected[this] val toTuple: Function[Tuple] = Tuple7.apply

  override def apply[H](f: Function[H]): F[H] = ap.apply7(fa, fb, fc, fd, fe, ff, fg)(f)
  override def ⊛[H](fh: F[H]) = new ApplicativeBuilderType[H](fa, fb, fc, fd, fe, ff, fg, fh)
}


private[scalaz] final class ApplicativeBuilder8[F[_], A, B, C, D, E, FF, G, H](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G], fh: F[H])
                                                                              (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[I] = (A, B, C, D, E, FF, G, H) => I
  type Tuple = (A, B, C, D, E, FF, G, H)
  type ApplicativeBuilderType[I] = ApplicativeBuilder9[F, A, B, C, D, E, FF, G, H, I]

  override protected[this] val toTuple: Function[Tuple] = Tuple8.apply

  override def apply[I](f: Function[I]): F[I] = ap.apply8(fa, fb, fc, fd, fe, ff, fg, fh)(f)
  override def ⊛[I](fi: F[I]) = new ApplicativeBuilderType[I](fa, fb, fc, fd, fe, ff, fg, fh, fi)
}


private[scalaz] final class ApplicativeBuilder9[F[_], A, B, C, D, E, FF, G, H, I](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G], fh: F[H], fi: F[I])
                                                                                 (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[J] = (A, B, C, D, E, FF, G, H, I) => J
  type Tuple = (A, B, C, D, E, FF, G, H, I)
  type ApplicativeBuilderType[J] = ApplicativeBuilder10[F, A, B, C, D, E, FF, G, H, I, J]

  override protected[this] val toTuple: Function[Tuple] = Tuple9.apply

  override def apply[J](f: Function[J]): F[J] = ap.apply9(fa, fb, fc, fd, fe, ff, fg, fh, fi)(f)
  override def ⊛[J](fj: F[J]) = new ApplicativeBuilderType[J](fa, fb, fc, fd, fe, ff, fg, fh, fi, fj)
}


private[scalaz] final class ApplicativeBuilder10[F[_], A, B, C, D, E, FF, G, H, I, J](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G], fh: F[H], fi: F[I], fj: F[J])
                                                                                     (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[K] = (A, B, C, D, E, FF, G, H, I, J) => K
  type Tuple = (A, B, C, D, E, FF, G, H, I, J)
  type ApplicativeBuilderType[K] = ApplicativeBuilder11[F, A, B, C, D, E, FF, G, H, I, J, K]

  override protected[this] val toTuple: Function[Tuple] = Tuple10.apply

  override def apply[K](f: Function[K]): F[K] = ap.apply10(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj)(f)
  override def ⊛[K](fk: F[K]) = new ApplicativeBuilderType[K](fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk)
}


private[scalaz] final class ApplicativeBuilder11[F[_], A, B, C, D, E, FF, G, H, I, J, K](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K])
                                                                                        (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] with ApplicativeBuilderOps[F] {
  type Function[L] = (A, B, C, D, E, FF, G, H, I, J, K) => L
  type Tuple = (A, B, C, D, E, FF, G, H, I, J, K)
  type ApplicativeBuilderType[L] = ApplicativeBuilder12[F, A, B, C, D, E, FF, G, H, I, J, K, L]

  override protected[this] val toTuple: Function[Tuple] = Tuple11.apply

  override def apply[L](f: Function[L]): F[L] = ap.apply11(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk)(f)
  override def ⊛[L](fl: F[L]) = new ApplicativeBuilderType[L](fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl)
}


private[scalaz] final class ApplicativeBuilder12[F[_], A, B, C, D, E, FF, G, H, I, J, K, L](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L])
                                                                                           (override protected[this] implicit val ap: Apply[F])
  extends ApplicativeBuilder[F] {
  type Function[M] = (A, B, C, D, E, FF, G, H, I, J, K, L) => M
  type Tuple = (A, B, C, D, E, FF, G, H, I, J, K, L)

  override protected[this] val toTuple: Function[Tuple] = Tuple12.apply

  override def apply[M](f: Function[M]): F[M] = ap.apply12(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl)(f)
}
