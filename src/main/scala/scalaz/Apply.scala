package scalaz

trait ApplyLike[F[_]] extends FunctorLike[F] { self => 
  def ap[A,B](fa: F[A])(f: F[A => B]): F[B]
  
  // derived functions
  def ap2[A,B,C](fa: F[A], fb: F[B])(f: F[(A,B) => C]): F[C] = 
    ap(fb)(ap(fa)(map(f)(_.curried)))
  def ap3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: F[(A,B,C) => D]): F[D] =
    ap(fc)(ap2(fa,fb)(map(f)(f => ((a:A,b:B) => (c:C) => f(a,b,c)))))
  def ap4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: F[(A,B,C,D) => E]): F[E] =
    ap(fd)(ap3(fa,fb,fc)(map(f)(f => ((a:A,b:B,c:C) => (d:D) => f(a,b,c,d)))))
  def ap5[A,B,C,D,E,R](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: F[(A,B,C,D,E) => R]): F[R] =
    ap(fe)(ap4(fa,fb,fc,fd)(map(f)(f => ((a:A,b:B,c:C,d:D) => (e:E) => f(a,b,c,d,e)))))

  override val syntax = new scalaz.syntax.ApplySyntax[F] {}
}
trait Apply[F[_]] extends ApplyLike[F] 
trait ApplyInstance[F[_]] extends Apply[F] with FunctorInstance[F]

