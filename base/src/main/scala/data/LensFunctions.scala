package scalaz
package data

import scala.language.experimental.macros

import typeclass.Functor

trait LensFunctions {
  def lens[S, T, A, B](sa: S => A)(sbt: S => B => T): Lens[S, T, A, B] = macro LensMacros.lens[S, T, A, B]
  def slens[S, A](sa: S => A)(sas: S => A => S): Lens[S, S, A, A] = macro LensMacros.slens[S, A]
}
