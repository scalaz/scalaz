package scalaz

trait ApplicativeLike[F[_]] extends PointedLike[F] with ApplyLike[F] { self => 
 
  // derived functions
  def map[A,B](fa: F[A])(f: A => B): F[B] = 
    ap(fa)(pure(f))
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = 
    ap2(fa, fb)(pure(f))
  def map3[A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = 
    map2(map2(fa, fb)((_,_)), fc)((ab,c) => f(ab._1, ab._2, c))
  def map4[A,B,C,D,E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D) => E): F[E] = 
    map2(map3(fa, fb, fc)((_,_,_)), fd)((t,d) => f(t._1,t._2,t._3,d)) 
  def map5[A,B,C,D,E,R](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: (A,B,C,D,E) => R): F[R] = 
    map2(map4(fa, fb, fc, fd)((_,_,_,_)), fe)((t,e) => f(t._1,t._2,t._3,t._4,e)) 
  def lift2[A,B,C](f: (A,B) => C): (F[A],F[B]) => F[C] = 
    map2(_,_)(f) 
  def lift3[A,B,C,D](f: (A,B,C) => D): (F[A],F[B],F[C]) => F[D] = 
    map3(_,_,_)(f) 
  def lift4[A,B,C,D,E](f: (A,B,C,D) => E): (F[A],F[B],F[C],F[D]) => F[E] = 
    map4(_,_,_,_)(f) 
  def lift5[A,B,C,D,E,R](f: (A,B,C,D,E) => R): (F[A],F[B],F[C],F[D],F[E]) => F[R] = 
    map5(_,_,_,_,_)(f) 

  // impls of sequence, traverse, etc

  override val syntax = new ApplicativeSyntax[F] {}
}
trait Applicative[F[_]] extends ApplicativeLike[F]
trait ApplicativeInstance[F[_]] extends Applicative[F] with ApplyInstance[F] with PointedInstance[F]

trait ApplicativeV[F[_],A] extends SyntaxV[F[A]] {  
  def map2[B,C](fb: F[B])(f: (A,B) => C)(implicit F: Applicative[F]) = F.map2(self,fb)(f)
  def pair[B](fb: F[B])(implicit F: Applicative[F]) = F.map2(self, fb)((_,_))
}
trait ToApplicativeSyntax extends ToApplySyntax with ToPointedSyntax { 
  implicit def applicative[F[_],A](v: F[A]) = (new ApplicativeSyntax[F] {}).applicativeV(v)
}
trait ApplicativeSyntax[F[_]] extends ApplySyntax[F] with PointedSyntax[F] { 
  implicit def applicativeV[A](v: F[A]) = new ApplicativeV[F,A] { def self = v }
  implicit def lift2V[A,B,C](f: (A,B) => C)(implicit F: Applicative[F]) = F.lift2(f)
  implicit def lift3V[A,B,C,D](f: (A,B,C) => D)(implicit F: Applicative[F]) = F.lift3(f)
}
