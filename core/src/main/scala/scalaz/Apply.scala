package scalaz

////
/**
 *
 */
////
trait Apply[F[_]] extends Functor[F] { self =>
  ////
  def ap[A,B](fa: => F[A])(f: => F[A => B]): F[B]

  // derived functions

  /**The composition of Applys `F` and `G`, `[x]F[G[x]]`, is a Apply */
  def compose[G[_]](implicit G0: Apply[G]): Apply[({type λ[α] = F[G[α]]})#λ] = new CompositionApply[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Applys `F` and `G`, `[x](F[x], G[x]])`, is a Apply */
  def product[G[_]](implicit G0: Apply[G]): Apply[({type λ[α] = (F[α], G[α])})#λ] = new ProductApply[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  def apF[A,B](f: => F[A => B]): F[A] => F[B] = ap(_)(f)
  def zip: Zip[F] =
    new Zip[F] {
      def zip[A, B](a: => F[A], b: => F[B]): F[(A, B)] =
        apply(a, b)((x, y) => (x, y))
    }

  def ap[A,B,C](fa: => F[A], fb: => F[B])(f: F[(A,B) => C]): F[C] =
    ap(fb)(ap(fa)(map(f)(_.curried)))
  def ap[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(f: F[(A,B,C) => D]): F[D] =
    ap(fc)(ap(fa,fb)(map(f)(f => ((a:A,b:B) => (c:C) => f(a,b,c)))))
  def ap[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: F[(A,B,C,D) => E]): F[E] =
    ap(fc, fd)(ap(fa,fb)(map(f)(f => ((a:A,b:B) => (c:C, d:D) => f(a,b,c,d)))))
  def ap[A,B,C,D,E,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(f: F[(A,B,C,D,E) => R]): F[R] =
    ap(fd, fe)(ap(fa,fb,fc)(map(f)(f => ((a:A,b:B,c:C) => (d:D, e:E) => f(a,b,c,d,e)))))
  def ap[A,B,C,D,E,FF, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF])(f: F[(A,B,C,D,E,FF) => R]): F[R] =
    ap(fd, fe, ff)(ap(fa,fb,fc)(map(f)(f => ((a:A,b:B,c:C) => (d:D, e:E, ff: FF) => f(a,b,c,d,e,ff)))))
  def ap[A,B,C,D,E,FF,G,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G])(f: F[(A,B,C,D,E,FF,G) => R]): F[R] =
    ap(fe, ff, fg)(ap(fa,fb,fc,fd)(map(f)(f => ((a:A,b:B,c:C,d: D) => (e:E, ff: FF, g: G) => f(a,b,c,d,e,ff,g)))))
  def ap[A,B,C,D,E,FF,G,H,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H])(f: F[(A,B,C,D,E,FF,G,H) => R]): F[R] =
    ap(fe, ff, fg, fh)(ap(fa,fb,fc,fd)(map(f)(f => ((a:A,b:B,c:C,d: D) => (e:E, ff: FF, g: G, h: H) => f(a,b,c,d,e,ff,g,h)))))

  @deprecated("given `F: Apply[F]` use `F(a,b)(f)` instead, or given `implicitly[Apply[F]]`, use `^(a,b)(f)`", "7")
  def map2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
    apply(fa,fb)(f)

  @deprecated("given `F: Apply[F]` use `F(a,b,c)(f)` instead, or given `implicitly[Apply[F]]`, use `^(a,b,c)(f)`", "7")
  def map3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: (A, B, C) => D): F[D] =
    apply(fa,fb,fc)(f)

  @deprecated("given `F: Apply[F]` use `F(a,b,c,d)(f)` instead, or given `implicitly[Apply[F]]`, use `^(a,b,c,d)(f)`", "7")
  def map4[A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: (A, B, C, D) => E): F[E] =
    apply(fa,fb,fc,fd)(f)

  def apply[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] = ap(fb)(map(fa)(f.curried))

  def apply[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(fa, fb)((_, _)), fc)((ab, c) => f(ab._1, ab._2, c))
  def apply[A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(fa, fb)((_, _)), apply(fc, fd)((_, _)))((t, d) => f(t._1, t._2, d._1, d._2))
  def apply[A, B, C, D, E, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(f: (A, B, C, D, E) => R): F[R] =
    apply(apply(fa, fb, fc)((_, _, _)), apply(fd, fe)((_, _)))((t, t2) => f(t._1, t._2, t._3, t2._1, t2._2))
  def apply[A, B, C, D, E, FF, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF])(f: (A, B, C, D, E, FF) => R): F[R] =
    apply(apply(fa, fb, fc)((_, _, _)), apply(fd, fe, ff)((_, _, _)))((t, t2) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3))
  def apply[A, B, C, D, E, FF, G, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G])(f: (A, B, C, D, E, FF, G) => R): F[R] =
    apply(apply(fa, fb, fc, fd)((_, _, _, _)), apply(fe, ff, fg)((_, _, _)))((t, t2) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3))
  def apply[A, B, C, D, E, FF, G, H, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H])(f: (A, B, C, D, E, FF, G, H) => R): F[R] =
    apply(apply(fa, fb, fc, fd)((_, _, _, _)), apply(fe, ff, fg, fh)((_, _, _, _)))((t, t2) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3, t2._4))
  def apply[A, B, C, D, E, FF, G, H, I, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I])(f: (A, B, C, D, E, FF, G, H, I) => R): F[R] =
    apply(apply(fa, fb, fc)((_, _, _)), apply(fd, fe, ff)((_, _, _)), apply(fg, fh, fi)((_, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3, t3._1, t3._2, t3._3))
  def apply[A, B, C, D, E, FF, G, H, I, J, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                          fi: => F[I], fj: => F[J])(f: (A, B, C, D, E, FF, G, H, I, J) => R): F[R] =
    apply(apply(fa, fb, fc)((_, _, _)), apply(fd, fe, ff)((_, _, _)), apply(fg, fh, fi, fj)((_, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3, t3._1, t3._2, t3._3, t3._4))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                          fi: => F[I], fj: => F[J], fk: => F[K])(f: (A, B, C, D, E, FF, G, H, I, J, K) => R): F[R] =
    apply(apply(fa, fb, fc)((_, _, _)), apply(fd, fe, ff, fg)((_, _, _, _)), apply(fh, fi, fj, fk)((_, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3, t2._4, t3._1, t3._2, t3._3, t3._4))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                          fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L])(f: (A, B, C, D, E, FF, G, H, I, J, K, L) => R): F[R] =
    apply(apply(fa, fb, fc, fd)((_, _, _, _)), apply(fe, ff, fg, fh)((_, _, _, _)), apply(fi, fj, fk, fl)((_, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3, t2._4, t3._1, t3._2, t3._3, t3._4))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, M, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                          fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M])(f: (A, B, C, D, E, FF, G, H, I, J, K, L, M) => R): F[R] =
    apply(apply(fa, fb, fc, fd)((_, _, _, _)), apply(fe, ff, fg, fh)((_, _, _, _)), apply(fi, fj, fk, fl, fm)((_, _, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3, t2._4, t3._1, t3._2, t3._3, t3._4, t3._5))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I],
                                          fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N])(f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N) => R): F[R] =
    apply(apply(fa, fb, fc, fd)((_, _, _, _)), apply(fe, ff, fg, fh, fi)((_, _, _, _, _)), apply(fj, fk, fl, fm, fn)((_, _, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3, t2._4, t2._5, t3._1, t3._2, t3._3, t3._4, t3._5))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E],
                                          ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K],
                                          fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O])(f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O) => R): F[R] =
    apply(apply(fa, fb, fc, fd, fe)((_, _, _, _, _)), apply(ff, fg, fh, fi, fj)((_, _, _, _, _)), apply(fk, fl, fm, fn, fo)((_, _, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t._5, t2._1, t2._2, t2._3, t2._4, t2._5, t3._1, t3._2, t3._3, t3._4, t3._5))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E],
                                          ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K],
                                          fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P])(f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P) => R): F[R] =
    apply(apply(fa, fb, fc, fd, fe)((_, _, _, _, _)), apply(ff, fg, fh, fi, fj)((_, _, _, _, _)), apply(fk, fl, fm, fn, fo, fp)((_, _, _, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t._5, t2._1, t2._2, t2._3, t2._4, t2._5, t3._1, t3._2, t3._3, t3._4, t3._5, t3._6))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E],
                                          ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K],
                                          fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q])(f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q) => R): F[R] =
    apply(apply(fa, fb, fc, fd, fe)((_, _, _, _, _)), apply(ff, fg, fh, fi, fj, fk)((_, _, _, _, _, _)), apply(fl, fm, fn, fo, fp, fq)((_, _, _, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t._5, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t3._1, t3._2, t3._3, t3._4, t3._5, t3._6))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E],
                                          ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: F[L], fm: => F[M],
                                          fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: F[RR])(f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR) => R): F[R] =
    apply(apply(fa, fb, fc, fd, fe, ff)((_, _, _, _, _, _)), apply(fg, fh, fi, fj, fk, fl)((_, _, _, _, _, _)), apply(fm, fn, fo, fp, fq, fr)((_, _, _, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t._5, t._6, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t3._1, t3._2, t3._3, t3._4, t3._5, t3._6))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E],
                                          ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: F[L], fm: => F[M],
                                          fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: F[RR], fs: F[S])(f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S) => R): F[R] =
    apply(apply(fa, fb, fc, fd, fe, ff)((_, _, _, _, _, _)), apply(fg, fh, fi, fj, fk, fl)((_, _, _, _, _, _)), apply(fm, fn, fo, fp, fq, fr, fs)((_, _, _, _, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t._5, t._6, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t3._1, t3._2, t3._3, t3._4, t3._5, t3._6, t3._7))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E],
                                          ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: F[L], fm: => F[M],
                                          fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: F[RR], fs: F[S], ft: F[T])(f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T) => R): F[R] =
    apply(apply(fa, fb, fc, fd, fe, ff)((_, _, _, _, _, _)), apply(fg, fh, fi, fj, fk, fl, fm)((_, _, _, _, _, _, _)), apply(fn, fo, fp, fq, fr, fs, ft)((_, _, _, _, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t._5, t._6, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t3._1, t3._2, t3._3, t3._4, t3._5, t3._6, t3._7))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T, U, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E],
                                          ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: F[L], fm: => F[M],
                                          fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: F[RR], fs: F[S], ft: F[T], fu: F[U])(f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T, U) => R): F[R] =
    apply(apply(fa, fb, fc, fd, fe, ff, fg)((_, _, _, _, _, _, _)), apply(fh, fi, fj, fk, fl, fm, fn)((_, _, _, _, _, _, _)), apply(fo, fp, fq, fr, fs, ft, fu)((_, _, _, _, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t3._1, t3._2, t3._3, t3._4, t3._5, t3._6, t3._7))
  def apply[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T, U, V, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E],
                                          ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: F[L], fm: => F[M],
                                          fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: F[RR], fs: F[S], ft: F[T], fu: F[U], fv: F[V])(f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T, U, V) => R): F[R] =
    apply(apply(fa, fb, fc, fd, fe, ff, fg)((_, _, _, _, _, _, _)), apply(fh, fi, fj, fk, fl, fm, fn)((_, _, _, _, _, _, _)), apply(fo, fp, fq, fr, fs, ft, fu, fv)((_, _, _, _, _, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t2._1, t2._2, t2._3, t2._4, t2._5, t2._6, t2._7, t3._1, t3._2, t3._3, t3._4, t3._5, t3._6, t3._7, t3._8))

  def tuple[A,B](fa: => F[A], fb: => F[B]): F[(A,B)] =
    apply(fa, fb)((_,_))
  def tuple[A,B,C](fa: => F[A], fb: => F[B], fc: F[C]): F[(A,B,C)] =
    apply(fa, fb, fc)((_,_,_))
  def tuple[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D]): F[(A,B,C,D)] =
    apply(fa, fb, fc, fd)((_,_,_,_))
  def tuple[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E]): F[(A,B,C,D,E)] =
    apply(fa, fb, fc, fd, fe)((_,_,_,_,_))
  def tuple[A,B,C,D,E,FF](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF]): F[(A,B,C,D,E,FF)] =
    apply(fa, fb, fc, fd, fe, ff)((_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G]): F[(A,B,C,D,E,FF,G)] =
    apply(fa, fb, fc, fd, fe, ff, fg)((_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H]): F[(A,B,C,D,E,FF,G,H)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh)((_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I]): F[(A,B,C,D,E,FF,G,H,I)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi)((_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J]): F[(A,B,C,D,E,FF,G,H,I,J)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj)((_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K]): F[(A,B,C,D,E,FF,G,H,I,J,K)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk)((_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L]): F[(A,B,C,D,E,FF,G,H,I,J,K,L)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl)((_,_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L,M](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M]): F[(A,B,C,D,E,FF,G,H,I,J,K,L,M)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl, fm)((_,_,_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L,M,N](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N]): F[(A,B,C,D,E,FF,G,H,I,J,K,L,M,N)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl, fm, fn)((_,_,_,_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O]): F[(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl, fm, fn, fo)((_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P]): F[(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp)((_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q]): F[(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq)((_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R]): F[(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr)((_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S]): F[(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs)((_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S,T](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T]): F[(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft)((_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U]): F[(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu)((_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))
  def tuple[A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L], fm: => F[M], fn: => F[N], fo: => F[O], fp: => F[P], fq: => F[Q], fr: => F[R], fs: => F[S], ft: => F[T], fu: => F[U], fv: => F[V]): F[(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)] =
    apply(fa, fb, fc, fd, fe, ff, fg, fh, fi, fj, fk, fl, fm, fn, fo, fp, fq, fr, fs, ft, fu, fv)((_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_))

  def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] =
    apply(_, _)(f)
  def lift3[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[D] =
    apply(_, _, _)(f)
  def lift4[A, B, C, D, E](f: (A, B, C, D) => E): (F[A], F[B], F[C], F[D]) => F[E] =
    apply(_, _, _, _)(f)
  def lift5[A, B, C, D, E, R](f: (A, B, C, D, E) => R): (F[A], F[B], F[C], F[D], F[E]) => F[R] =
    apply(_, _, _, _, _)(f)
  def lift6[A, B, C, D, E, FF, R](f: (A, B, C, D, E, FF) => R): (F[A], F[B], F[C], F[D], F[E], F[FF]) => F[R] =
    apply(_, _, _, _, _, _)(f)
  def lift7[A, B, C, D, E, FF, G, R](f: (A, B, C, D, E, FF, G) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G]) => F[R] =
    apply(_, _, _, _, _, _, _)(f)
  def lift8[A, B, C, D, E, FF, G, H, R](f: (A, B, C, D, E, FF, G, H) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H]) => F[R] =
    apply(_, _, _, _, _, _, _, _)(f)
  def lift9[A, B, C, D, E, FF, G, H, I, R](f: (A, B, C, D, E, FF, G, H, I) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _)(f)
  def lift10[A, B, C, D, E, FF, G, H, I, J, R](f: (A, B, C, D, E, FF, G, H, I, J) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _)(f)
  def lift11[A, B, C, D, E, FF, G, H, I, J, K, R](f: (A, B, C, D, E, FF, G, H, I, J, K) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _)(f)
  def lift12[A, B, C, D, E, FF, G, H, I, J, K, L, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _)(f)
  def lift13[A, B, C, D, E, FF, G, H, I, J, K, L, M, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L, M) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L], F[M]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _, _)(f)
  def lift14[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L], F[M], F[N]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _)(f)
  def lift15[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L], F[M], F[N], F[O]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)(f)
  def lift16[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L], F[M], F[N], F[O], F[P]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)(f)
  def lift17[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L], F[M], F[N], F[O], F[P], F[Q]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)(f)
  def lift18[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L], F[M], F[N], F[O], F[P], F[Q], F[RR]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)(f)
  def lift18[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L], F[M], F[N], F[O], F[P], F[Q], F[RR], F[S]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)(f)
  def lift19[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L], F[M], F[N], F[O], F[P], F[Q], F[RR], F[S], F[T]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)(f)
  def lift20[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T, U, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T, U) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L], F[M], F[N], F[O], F[P], F[Q], F[RR], F[S], F[T], F[U]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)(f)
  def lift21[A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T, U, V, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L, M, N, O, P, Q, RR, S, T, U, V) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L], F[M], F[N], F[O], F[P], F[Q], F[RR], F[S], F[T], F[U], F[V]) => F[R] =
    apply(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)(f)

  ////
  val applySyntax = new scalaz.syntax.ApplySyntax[F] { def F = Apply.this }
}

object Apply {
  @inline def apply[F[_]](implicit F: Apply[F]): Apply[F] = F

  ////

  ////
}
