package scalaz
package syntax
package effect

import scalaz.effect.IO

sealed abstract class IdOps[A] extends Ops[A] {

  final def put(implicit S: Show[A]): IO[Unit] =
    IO.put(self)

  final def putLn(implicit S: Show[A]): IO[Unit] =
    IO.putLn(self)

  /** Safe version of tap. */
  final def tap[B](f: A => IO[B]): IO[A] =
    for { _ <- f(self) } yield self

}

trait ToIdOps {
  implicit def ToEffectIdOps[A](a: A): IdOps[A] = new IdOps[A] {
    def self: A = a
  }
}
