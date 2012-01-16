package scalaz
package typelevel.formatters.unified

import UnionTypes._
import typelevel.Formatter._

trait UnionFormat extends Format {
  type D <: Disj
  type Source = Union[D]
}

object UnionFormat {
  def deunion[U <: UnionFormat, S](
    unionFormat: U)(implicit ev: !![S] <:< or[U#D]
  ): Format{type Source = S} = new Format {
    type Source = S
    def apply(x: Source) = unionFormat(x.union[unionFormat.D])
  }
}
