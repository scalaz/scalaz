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

  override val syntax = new ApplicativeSyntax[F] {}
}
trait Apply[F[_]] extends ApplyLike[F] 
trait ApplyInstance[F[_]] extends Apply[F] with FunctorInstance[F]

trait ApplyV[F[_],A] extends SyntaxV[F[A]] { 
  def <*>[B](f: F[A => B])(implicit F: Apply[F]) = F.ap(self)(f)
}

trait ToApplySyntax extends ToFunctorSyntax { 
  implicit def apply[F[_],A](v: F[A]) = (new ApplySyntax[F] {}).applyV(v)
}
trait ApplySyntax[F[_]] extends FunctorSyntax[F] { 
  implicit def applyV[A](v: F[A]) = new ApplyV[F,A] { def self = v }
}
