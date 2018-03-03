package scalaz

import std.AllInstances._
import syntax.either._
import syntax.functor._
import Isomorphism.{ <~>, IsoFunctorTemplate }

object MonadErrorTest extends SpecLite {

  trait Decoder[A] {
    def decode(s: String): String \/ A
  }
  object Decoder {
    type Out[a] = String \/ a

    @inline def apply[A](implicit A: Decoder[A]): Decoder[A] = A
    @inline def instance[A](f: String => String \/ A): Decoder[A] = new Decoder[A] {
      override def decode(s: String): String \/ A = f(s)
    }

    implicit val string: Decoder[String] = instance(_.right)

    implicit val isoReaderT: Decoder <~> ReaderT[Out, String, ?] =
      new IsoFunctorTemplate[Decoder, ReaderT[Out, String, ?]] {
        def from[A](fa: ReaderT[Out, String, A]) = instance(fa.run(_))
        def to[A](fa: Decoder[A]) = ReaderT[Out, String, A](fa.decode)
      }

    implicit val monad: MonadError[Decoder, String] =
      MonadError.fromIsoWithMonadError[Decoder, String, Out, String]
  }

  "fromIsoWithMonadError" in {
    Decoder[String].map(_.toUpperCase).decode("hello") must_=== "HELLO".right
  }

}
