package scalaz

trait PointedLike[F[_]] extends FunctorLike[F] { 
  def pure[A](a: => A): F[A]
}
trait Pointed[F[_]] extends PointedLike[F]
trait PointedInstance[F[_]] extends FunctorInstance[F] with Pointed[F]

trait PointedV[A] extends SyntaxV[A] {
  def pure[F[_]](implicit F: Pointed[F]) = F.pure(self)
}
trait ToPointedSyntax extends ToFunctorSyntax { 
  implicit def pointed[A](v: A) = new PointedV[A] { def self = v }
}
trait PointedSyntax[F[_]] extends FunctorSyntax[F] { 
  implicit def pointedV[A](v: A) = new PointedV[A] { def self = v }
}
