package scalaz
package typeclass

trait Bind[M[_]] {
  def apply: Apply[M]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def tapM[A, B](ma: M[A])(f: A => M[B]): M[A]
  def flatten[A](ma: M[M[A]]): M[A]
}

object Bind extends BindInstances with BindFunctions with BindSyntax {
  trait TapM
  def apply[M[_]](implicit M: Bind[M]): Bind[M] = M

  object syntax extends BindSyntax
}
