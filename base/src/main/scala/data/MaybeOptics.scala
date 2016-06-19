package scalaz
package data

import Disjunction._
import Maybe._
import Optic._

trait MaybeOptics {
  def Just[A, B]: Prism[Maybe[A], Maybe[B], A, B] =
    prism[Maybe[A], Maybe[B], A, B](just(_))(maybe[A, Maybe[B] \/ A](-\/(empty[B]))(\/-(_)))

  def Empty[A, B]: Prism_[Maybe[A], Unit] =
    prismM[Maybe[A], Unit, Unit](_ => empty)(maybe[A, Maybe[Unit]](just(()))(_ => empty))
}
