package scalaz
package typeclass

trait BindClass[M[_]] extends Bind[M] with ApplyClass[M] {
  final def bind: Bind[M] = this
}

object BindClass {
  trait Template[M[_]] extends BindClass[M] with Ap[M]

  trait Ap[M[_]] { self: BindClass[M] =>
    override def ap[A, B](fa: M[A])(f: M[A => B]): M[B] = flatMap(f)(map(fa))
  }

}
