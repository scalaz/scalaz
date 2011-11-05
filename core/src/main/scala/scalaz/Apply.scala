package scalaz

trait Apply[F[_]] extends Functor[F] { self =>
  ////
  def ap[A,B](fa: F[A])(f: F[A => B]): F[B]

  // derived functions

  def apF[A,B](f: F[A => B]): F[A] => F[B] = ap(_)(f)

  def ap2[A,B,C](fa: F[A], fb: F[B])(f: F[(A,B) => C]): F[C] =
    ap(fb)(ap(fa)(map(f)(_.curried)))
  def ap3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: F[(A,B,C) => D]): F[D] =
    ap(fc)(ap2(fa,fb)(map(f)(f => ((a:A,b:B) => (c:C) => f(a,b,c)))))
  def ap4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: F[(A,B,C,D) => E]): F[E] =
    ap(fd)(ap3(fa,fb,fc)(map(f)(f => ((a:A,b:B,c:C) => (d:D) => f(a,b,c,d)))))
  def ap5[A,B,C,D,E,R](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: F[(A,B,C,D,E) => R]): F[R] =
    ap(fe)(ap4(fa,fb,fc,fd)(map(f)(f => ((a:A,b:B,c:C,d:D) => (e:E) => f(a,b,c,d,e)))))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ap(fb)(map(fa)(f.curried))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    map2(map2(fa, fb)((_, _)), fc)((ab, c) => f(ab._1, ab._2, c))
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    map2(map3(fa, fb, fc)((_, _, _)), fd)((t, d) => f(t._1, t._2, t._3, d))
  def map5[A, B, C, D, E, R](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: (A, B, C, D, E) => R): F[R] =
    map2(map4(fa, fb, fc, fd)((_, _, _, _)), fe)((t, e) => f(t._1, t._2, t._3, t._4, e))
  def lift2[A, B, C](f: (A, B) => C): (F[A], F[B]) => F[C] =
    map2(_, _)(f)
  def lift3[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[D] =
    map3(_, _, _)(f)
  def lift4[A, B, C, D, E](f: (A, B, C, D) => E): (F[A], F[B], F[C], F[D]) => F[E] =
    map4(_, _, _, _)(f)
  def lift5[A, B, C, D, E, R](f: (A, B, C, D, E) => R): (F[A], F[B], F[C], F[D], F[E]) => F[R] =
    map5(_, _, _, _, _)(f)

  ////
  val applySyntax = new scalaz.syntax.ApplySyntax[F] {}
}

object Apply {
  def apply[F[_]](implicit F: Apply[F]): Apply[F] = F

  ////

  ////
}

