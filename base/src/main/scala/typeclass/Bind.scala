package scalaz
package typeclass

trait Bind[M[_]] {
  def apply: Apply[M]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
}

object Bind extends BindInstances with BindFunctions with BindSyntax {
  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F
}
