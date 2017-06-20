package scalaz
package data

import typeclass.Compose

final case class Endo[=>:[_, _], A](run: A =>: A) {
  final def compose(that: Endo[=>:, A])(implicit F: Compose[=>:]): Endo[=>:, A] =
    Endo[=>:, A](F.compose(run, that.run))
}
