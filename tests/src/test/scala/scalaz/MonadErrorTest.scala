package scalaz

import std.AllInstances._
import syntax.either._
import syntax.monadError._
import Isomorphism.{ <~>, IsoFunctorTemplate }

object MonadErrorTest extends SpecLite {

  trait Decoder[A] {
    def decode(s: String): String \/ A
  }
  object Decoder {
    @inline def apply[A](implicit A: Decoder[A]): Decoder[A] = A
    @inline def instance[A](f: String => String \/ A): Decoder[A] = new Decoder[A] {
      override def decode(s: String): String \/ A = f(s)
    }

    // type aliases are needed to help with type inference, even kind-projector
    // fails us...
    type Out[a] = String \/ a
    type MT[a] = ReaderT[String, Out, a]
    implicit val string: Decoder[String] = instance(_.right)
    implicit val isoReaderT: Decoder <~> MT =
      new IsoFunctorTemplate[Decoder, MT] {
        def from[A](fa: MT[A]) = instance(fa.run(_))
        def to[A](fa: Decoder[A]) = ReaderT[String, Out, A](fa.decode)
      }

    implicit val monad: MonadError[Decoder, String] = MonadError.fromIso(isoReaderT)
  }

  "fromIsoWithMonadError" in {
    Decoder[String].map(_.toUpperCase).decode("hello") must_=== "HELLO".right
  }

  "emap syntax" in {
    def f(s: String): String \/ Char =
      if (s.length == 1) s(0).right
      else s"not a char: $s".left

    Decoder[String].emap(f).decode("hello") must_=== "not a char: hello".left
    Decoder[String].emap(f).decode("g") must_=== 'g'.right
  }

}
