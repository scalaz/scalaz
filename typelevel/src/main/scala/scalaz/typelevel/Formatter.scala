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

  trait Format {
    type Source
    def apply(s: Source): String
  }

  type Fmt[T] = Format { type Source = T }

  object javaFormatter {
    def write(fmt: String, arg: Object): String =
      fmt format arg
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

  object all
    extends Formatters
    with formatters.AllFormatters
    with formatters.unified.AllFormatters

}

// vim: expandtab:ts=2:sw=2

