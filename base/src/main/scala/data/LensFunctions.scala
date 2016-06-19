package scalaz
package data

import typeclass.Functor

trait LensFunctions {
  def lens[S, T, A, B](sa: S => A)(sbt: S => B => T): Lens[S, T, A, B] = new Lens[S, T, A, B] {
    def apply[F[_]](afb: A => F[B])(implicit F: Functor[F]): S => F[T] = s => F.map(afb(sa(s)))(sbt(s))
  }

  def slens[S, A](sa: S => A)(sas: S => A => S): Lens[S, S, A, A] = lens[S, S, A, A](sa)(sas)
}

