package scalaz
package syntax
package std

trait IterableV[A] extends SyntaxV[Iterable[A]] {}

trait ToIterableV {
  implicit def ToIterableVFromIterable[A](a: Iterable[A]): IterableV[A] = new IterableV[A] {
    val self = a
  }
}