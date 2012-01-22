package scalaz
package typelevel

import java.{lang => jl, util => ju}

import Typelevel._

trait Formatter[Params <: HList] extends (Params => String) { self =>

  import Formatter._

  def format = apply _

  def ::[T](f: Fmt[T]): Formatter[HCons[_ <: T, Params]] =
    new Formatter[HCons[_ <: T, Params]] {
      def apply(params: HCons[_ <: T, Params]): String = f(params.head) + self(params.tail)
    }

  def ::(s: String): Formatter[Params] =
    new Formatter[Params] {
      def apply(params: Params): String = s + self(params)
    }

  def :<:[H, T <: HList](f: Fmt[H])(implicit ev: Params <:< HCons[_ <: H, T]): Formatter[Params] =
    new Formatter[Params] {
      def apply(params: Params): String = f(params.head) + self(params)
    }

  // Structural type is necessary here, because `::` does not override the `::`
  // in `Formatter`.
  def :<:(s: String): Formatter[Params] =
    new Formatter[Params] { spec =>
      def apply(params: Params): String = s + self(params)

      def ::[H, T <: HList](f: Fmt[H])(implicit ev: Params <:< HCons[_ <: H, T]): Formatter[Params] =
        new Formatter[Params] {
          def apply(params: Params): String = f(params.head) + spec(params)
        }
    }

}

trait Formatters {

  trait Fmt[Source] {
    def apply(s: Source): String
  }

  def FNil: Formatter[HNil] = new Formatter[HNil] {
    def apply(params: HNil) = ""
  }

  implicit def format2Formatter[T](f: Fmt[T]): Formatter[HCons[_ <: T, HNil]] =
    new Formatter[HCons[_ <: T, HNil]] {
      def apply(params: HCons[_ <: T, HNil]) = f(params.head)
    }

  implicit def string2Formatter(s: String): Formatter[HNil] =
    new Formatter[HNil] {
      def apply(params: HNil) = s
    }

}

object Formatter extends Formatters {

  import formatters._

  object all
    extends Formatters
    with General
    with JavaLike
    with String
    with unified.Numeric
    with unified.String

}

// vim: expandtab:ts=2:sw=2

