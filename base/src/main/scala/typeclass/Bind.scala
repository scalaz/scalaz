package scalaz
package typeclass

trait Bind[M[_]] {
  def apply: Apply[M]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
}

object Bind extends BindInstances with BindFunctions {
  def apply[F[_]](implicit F: Bind[F]): Bind[F] = F

  object syntax extends BindSyntax
}
