package scalaz
package typelevel.formatters.unified

import UnionTypes._
import typelevel.Formatter._

trait UnionFormat extends Format { self =>
  type D <: Disj
  type Source = Union[D]

  def deunion[S](implicit ev: S ∈ D): Fmt[S] = new Format {
    type Source = S
    def apply(s: S) = self.apply(s.union)
  }
}
