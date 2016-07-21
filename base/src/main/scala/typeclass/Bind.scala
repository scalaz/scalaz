package scalaz
package typeclass

trait Bind[M[_]] {
  def apply: Apply[M]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def flatten[A](ma: M[M[A]]): M[A]
}

object Bind extends BindInstances with BindFunctions {

  trait FlatMap[M[_]] extends Alt[FlatMap[M]] { self: Bind[M] =>
    override def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    override def flatten[A](ma: M[M[A]]): M[A] = flatMap(ma)(identity)
  }
  trait Flatten[M[_]] extends Alt[Flatten[M]] { self: Bind[M] =>
    override def flatten[A](ma: M[M[A]]): M[A]
    override def flatMap[A, B](ma: M[A])(f: (A) => M[B]): M[B] = flatten(apply.functor.map(ma)(f))
  }
  trait Alt[D <: Alt[D]] { self: D => }

  def apply[M[_]](implicit M: Bind[M]): Bind[M] = M

  object syntax extends BindSyntax
}
