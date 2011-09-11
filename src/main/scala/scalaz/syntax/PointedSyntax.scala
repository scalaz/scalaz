package scalaz
package syntax


trait PointedV[A] extends SyntaxV[A] {
  def pure[F[_]](implicit F: Pointed[F]) = F.pure(self)
}

trait ToPointedSyntax extends ToFunctorSyntax {
  implicit def pointed[A](v: A) = new PointedV[A] {
    def self = v
  }
}

trait PointedSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def pointedV[A](v: A) = new PointedV[A] {
    def self = v
  }
}
