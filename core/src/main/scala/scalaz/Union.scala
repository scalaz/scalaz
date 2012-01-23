package scalaz

/**
 * Union types using Curry-Howard isomorphism
 *
 * @see [[http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/]]
 * @see [[http://en.wikipedia.org/wiki/Curry-Howard_correspondence]]
 */
trait UnionTypes {

  type ![A] = A => Nothing
  type !![A] = ![![A]]

  trait Disj { self =>
    type D
    type t[S] = Disj {
      type D = self.D with ![S]
    }
  }

  type t[T] = {
    type t[S] = (Disj { type D = ![T] })#t[S]
  }

  type or[T <: Disj] = ![T#D]

  type Contains[S, T <: Disj] = !![S] <:< or[T]
  type ∈[S, T <: Disj] = Contains[S, T]

  sealed trait Union[T] {
    val value: Any
  }

  case class Converter[S](s: S) {
    def union[T <: Disj](implicit ev: Contains[S, T]): Union[T] =
      new Union[T] {
        val value = s
      }
  }

  implicit def any2Converter[S](s: S): Converter[S] = Converter[S](s)

}

object UnionTypes extends UnionTypes

// vim: expandtab:ts=2:sw=2

