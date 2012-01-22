package scalaz
package typelevel.formatters.unified

import UnionTypes._
import typelevel.Formatter._

trait UnionFormat[Dis <: Disj] extends Fmt[Union[Dis]] { self =>
  type D = Dis

  def deunion[S](implicit ev: S ∈ Dis): Fmt[S] = new Fmt[S] {
    def apply(s: S) = self.apply(s.union)
  }

  def of[S](implicit ev: S ∈ Dis): Fmt[S] = deunion

}
