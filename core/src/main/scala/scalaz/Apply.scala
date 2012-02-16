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

  def apF[A,B](f: => F[A => B]): F[A] => F[B] = ap(_)(f)

  def ap2[A,B,C](fa: => F[A], fb: => F[B])(f: F[(A,B) => C]): F[C] =
    ap(fb)(ap(fa)(map(f)(_.curried)))
  def ap3[A,B,C,D](fa: => F[A], fb: => F[B], fc: => F[C])(f: F[(A,B,C) => D]): F[D] =
    ap(fc)(ap2(fa,fb)(map(f)(f => ((a:A,b:B) => (c:C) => f(a,b,c)))))
  def ap4[A,B,C,D,E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: F[(A,B,C,D) => E]): F[E] =
    ap2(fc, fd)(ap2(fa,fb)(map(f)(f => ((a:A,b:B) => (c:C, d:D) => f(a,b,c,d)))))
  def ap5[A,B,C,D,E,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(f: F[(A,B,C,D,E) => R]): F[R] =
    ap2(fd, fe)(ap3(fa,fb,fc)(map(f)(f => ((a:A,b:B,c:C) => (d:D, e:E) => f(a,b,c,d,e)))))
  def ap6[A,B,C,D,E,FF, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF])(f: F[(A,B,C,D,E,FF) => R]): F[R] =
    ap3(fd, fe, ff)(ap3(fa,fb,fc)(map(f)(f => ((a:A,b:B,c:C) => (d:D, e:E, ff: FF) => f(a,b,c,d,e,ff)))))
  def ap7[A,B,C,D,E,FF,G,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G])(f: F[(A,B,C,D,E,FF,G) => R]): F[R] =
    ap3(fe, ff, fg)(ap4(fa,fb,fc,fd)(map(f)(f => ((a:A,b:B,c:C,d: D) => (e:E, ff: FF, g: G) => f(a,b,c,d,e,ff,g)))))
  def ap8[A,B,C,D,E,FF,G,H,R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H])(f: F[(A,B,C,D,E,FF,G,H) => R]): F[R] =
    ap4(fe, ff, fg, fh)(ap4(fa,fb,fc,fd)(map(f)(f => ((a:A,b:B,c:C,d: D) => (e:E, ff: FF, g: G, h: H) => f(a,b,c,d,e,ff,g,h)))))

  def map2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] = ap(fb)(map(fa)(f.curried))

  def map3[A, B, C, D](fa: => F[A], fb: => F[B], fc: => F[C])(f: (A, B, C) => D): F[D] =
    map2(map2(fa, fb)((_, _)), fc)((ab, c) => f(ab._1, ab._2, c))
  def map4[A, B, C, D, E](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D])(f: (A, B, C, D) => E): F[E] =
    map2(map2(fa, fb)((_, _)), map2(fc, fd)((_, _)))((t, d) => f(t._1, t._2, d._1, d._2))
  def map5[A, B, C, D, E, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E])(f: (A, B, C, D, E) => R): F[R] =
    map2(map3(fa, fb, fc)((_, _, _)), map2(fd, fe)((_, _)))((t, t2) => f(t._1, t._2, t._3, t2._1, t2._2))
  def map6[A, B, C, D, E, FF, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF])(f: (A, B, C, D, E, FF) => R): F[R] =
    map2(map3(fa, fb, fc)((_, _, _)), map3(fd, fe, ff)((_, _, _)))((t, t2) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3))
  def map7[A, B, C, D, E, FF, G, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G])(f: (A, B, C, D, E, FF, G) => R): F[R] =
    map2(map4(fa, fb, fc, fd)((_, _, _, _)), map3(fe, ff, fg)((_, _, _)))((t, t2) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3))
  def map8[A, B, C, D, E, FF, G, H, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D], fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H])(f: (A, B, C, D, E, FF, G, H) => R): F[R] =
    map2(map4(fa, fb, fc, fd)((_, _, _, _)), map4(fe, ff, fg, fh)((_, _, _, _)))((t, t2) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3, t2._4))
  def map9[A, B, C, D, E, FF, G, H, I, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H], fi: => F[I])(f: (A, B, C, D, E, FF, G, H, I) => R): F[R] =
    map3(map3(fa, fb, fc)((_, _, _)), map3(fd, fe, ff)((_, _, _)), map3(fg, fh, fi)((_, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3, t3._1, t3._2, t3._3))
  def map10[A, B, C, D, E, FF, G, H, I, J, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                          fi: => F[I], fj: => F[J])(f: (A, B, C, D, E, FF, G, H, I, J) => R): F[R] =
    map3(map3(fa, fb, fc)((_, _, _)), map3(fd, fe, ff)((_, _, _)), map4(fg, fh, fi, fj)((_, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3, t3._1, t3._2, t3._3, t3._4))
  def map11[A, B, C, D, E, FF, G, H, I, J, K, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                          fi: => F[I], fj: => F[J], fk: => F[K])(f: (A, B, C, D, E, FF, G, H, I, J, K) => R): F[R] =
    map3(map3(fa, fb, fc)((_, _, _)), map4(fd, fe, ff, fg)((_, _, _, _)), map4(fh, fi, fj, fk)((_, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t2._1, t2._2, t2._3, t2._4, t3._1, t3._2, t3._3, t3._4))
  def map12[A, B, C, D, E, FF, G, H, I, J, K, L, R](fa: => F[A], fb: => F[B], fc: => F[C], fd: => F[D],
                                          fe: => F[E], ff: => F[FF], fg: => F[G], fh: => F[H],
                                          fi: => F[I], fj: => F[J], fk: => F[K], fl: => F[L])(f: (A, B, C, D, E, FF, G, H, I, J, K, L) => R): F[R] =
    map3(map4(fa, fb, fc, fd)((_, _, _, _)), map4(fe, ff, fg, fh)((_, _, _, _)), map4(fi, fj, fk, fl)((_, _, _, _)))((t, t2, t3) => f(t._1, t._2, t._3, t._4, t2._1, t2._2, t2._3, t2._4, t3._1, t3._2, t3._3, t3._4))

  def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] =
    map2(_, _)(f)
  def lift3[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[D] =
    map3(_, _, _)(f)
  def lift4[A, B, C, D, E](f: (A, B, C, D) => E): (F[A], F[B], F[C], F[D]) => F[E] =
    map4(_, _, _, _)(f)
  def lift5[A, B, C, D, E, R](f: (A, B, C, D, E) => R): (F[A], F[B], F[C], F[D], F[E]) => F[R] =
    map5(_, _, _, _, _)(f)
  def lift6[A, B, C, D, E, FF, R](f: (A, B, C, D, E, FF) => R): (F[A], F[B], F[C], F[D], F[E], F[FF]) => F[R] =
    map6(_, _, _, _, _, _)(f)
  def lift7[A, B, C, D, E, FF, G, R](f: (A, B, C, D, E, FF, G) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G]) => F[R] =
    map7(_, _, _, _, _, _, _)(f)
  def lift8[A, B, C, D, E, FF, G, H, R](f: (A, B, C, D, E, FF, G, H) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H]) => F[R] =
    map8(_, _, _, _, _, _, _, _)(f)
  def lift9[A, B, C, D, E, FF, G, H, I, R](f: (A, B, C, D, E, FF, G, H, I) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I]) => F[R] =
    map9(_, _, _, _, _, _, _, _, _)(f)
  def lift10[A, B, C, D, E, FF, G, H, I, J, R](f: (A, B, C, D, E, FF, G, H, I, J) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J]) => F[R] =
    map10(_, _, _, _, _, _, _, _, _, _)(f)
  def lift11[A, B, C, D, E, FF, G, H, I, J, K, R](f: (A, B, C, D, E, FF, G, H, I, J, K) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K]) => F[R] =
    map11(_, _, _, _, _, _, _, _, _, _, _)(f)
  def lift12[A, B, C, D, E, FF, G, H, I, J, K, L, R](f: (A, B, C, D, E, FF, G, H, I, J, K, L) => R): (F[A], F[B], F[C], F[D], F[E], F[FF], F[G], F[H], F[I], F[J], F[K], F[L]) => F[R] =
    map12(_, _, _, _, _, _, _, _, _, _, _, _)(f)

  ////
  val applySyntax = new scalaz.syntax.ApplySyntax[F] {}
}

object Apply {
  @inline def apply[F[_]](implicit F: Apply[F]): Apply[F] = F

  ////

  ////
}

