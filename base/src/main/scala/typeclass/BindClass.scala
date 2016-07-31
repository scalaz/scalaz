package scalaz
package typeclass

trait BindClass[M[_]] extends Bind[M] with ApplyClass[M] {
  final def bind: Bind[M] = this
}

object BindClass {
  trait Template[M[_]] extends BindClass[M] with Ap[M]

  trait Ap[M[_]] { self: Bind[M] with Apply[M] with Functor[M] =>
    override def ap[A, B](fa: M[A])(f: M[A => B]): M[B] = flatMap(f)(map(fa))
  }

  trait FlatMap[M[_]] extends Alt[FlatMap[M]] { self: Bind[M] =>
    override def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    override def flatten[A](ma: M[M[A]]): M[A] = flatMap(ma)(identity)
  }
  trait Flatten[M[_]] extends Alt[Flatten[M]] { self: Bind[M] =>
    override def flatten[A](ma: M[M[A]]): M[A]
    override def flatMap[A, B](ma: M[A])(f: (A) => M[B]): M[B] = flatten(apply.functor.map(ma)(f))
  }

  trait Alt[D <: Alt[D]] { self: D => }
}
