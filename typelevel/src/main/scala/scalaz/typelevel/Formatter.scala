package scalaz
package typelevel

import java.{lang => jl, util => ju}

import scalaz.syntax.semigroup._
import UnionTypes._

trait Formatter[Params <: HList, R] extends (Params => R) { self =>

  import Formatter._

  implicit val semigroup: Semigroup[R]

  def format = apply _

  def update[P <: HList](f: P => R) = Formatter[P, R](f)

  def ::[T](f: Format[T, R]): Formatter[HCons[_ <: T, Params], R] =
    update(params => f(params.head) |+| self(params.tail))

  def ::(r: R): Formatter[Params, R] =
    update(params => r |+| self(params))

  def :<:[H, T <: HList](f: Format[H, R])(implicit ev: Params <:< HCons[_ <: H, T]): Formatter[Params, R] =
    update(params => f(params.head) |+| self(params))

  // Structural type is necessary here, because `::` does not override the `::`
  // in `Formatter`.
  def :<:(s: R): Formatter[Params, R] =
    new Formatter[Params, R] { spec =>
      implicit val semigroup = self.semigroup

      def apply(params: Params): R = s |+| self(params)

      def ::[H, T <: HList](f: Format[H, R])(implicit ev: Params <:< HCons[_ <: H, T]): Formatter[Params, R] =
        self.update[Params](params => f(params.head) |+| spec(params))
    }

}

object Format {

  def apply[T, R](f: T => R): Format[T, R] = new Format[T, R] {
    def apply(t: T) = f(t)
  }

}

sealed trait Format[T, R] {

  def apply(t: T): R

}

trait UnionFormat[D <: Disj, R] extends Format[Union[D], R] {

  type T = D

  def deunion[S](implicit ev: S ∈ D) = Format[S, R](s => apply(s.union))

  def of[S](implicit ev: S ∈ D): Format[S, R] = deunion

}

class SimpleUnionFormat[D <: Disj, R](f: Any => R) extends UnionFormat[D, R] {

  def apply(u: Union[D]) = f(u.value)

}

trait Formatters {

  implicit def format2Formatter[T, R : Semigroup](f: Format[T, R]) =
    Formatter[HCons[_ <: T, HNil], R](params => f(params.head))

  implicit def any2Formatter[R : Semigroup](r: R) =
    Formatter[HNil, R](_ => r)

}

object Formatter extends Formatters {

  private[typelevel] def apply[Params <: HList, R : Semigroup](f: Params => R): Formatter[Params, R] = new Formatter[Params, R] {

    val semigroup = Semigroup[R]

    def apply(params: Params) = f(params)

  }

  import formatters._

  object all
    extends Formatters
    with string.General
    with string.Strings
    with string.Numeric

}

// vim: expandtab:ts=2:sw=2
