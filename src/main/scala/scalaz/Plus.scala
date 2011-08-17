package scalaz

trait PlusLike[F[_]] extends FunctorLike[F] {
  def plus[A](a: F[A], b: => F[A]): F[A]
  def empty[A]: F[A]
}
trait Plus[F[_]] extends PlusLike[F]
trait PlusInstance[F[_]] extends Plus[F] with FunctorInstance[F]

trait PlusV[F[_],A] extends FunctorV[F,A] { 
  def <+>(other: => F[A])(implicit F: Plus[F]) = F.plus(self, other)
}
trait ToPlusSyntax extends ToFunctorSyntax { 
  implicit def plus[F[_],A](v: F[A]) = (new PlusSyntax[F] {}).plusV(v) 
}
trait PlusSyntax[F[_]] extends FunctorSyntax[F] { 
  implicit def plusV[A](v: F[A]) = new PlusV[F,A] { def self = v }
}

