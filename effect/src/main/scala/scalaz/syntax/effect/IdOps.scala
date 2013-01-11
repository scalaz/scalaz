package scalaz
package syntax
package effect

import scalaz.effect.IO

trait IdOps[A] extends Ops[A] {

  /** Safe version of tap. */
  final def tap[B](f: A => IO[B]): IO[A] =
    for { _ <- f(self) } yield self

}

trait ToIdOps {
  implicit def ToIdOps[A](a: A): IdOps[A] = new IdOps[A] {
    def self: A = a
  }
}
