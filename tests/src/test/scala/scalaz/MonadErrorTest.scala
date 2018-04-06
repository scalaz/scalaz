package scalaz

import std.AllInstances._
import syntax.either._
import syntax.monadError._
import Isomorphism.{ <~>, IsoFunctorTemplate }

object MonadErrorTest extends SpecLite {

  trait Decoder[A] {
    def decode(s: String): Int \/ A
  }
  object Decoder {
    @inline def apply[A](implicit A: Decoder[A]): Decoder[A] = A
    @inline def instance[A](f: String => Int \/ A): Decoder[A] = new Decoder[A] {
      override def decode(s: String): Int \/ A = f(s)
    }

    implicit val string: Decoder[String] = instance(_.right)

    type In = String
    type Out[a] = Int \/ a
    type MT[a] = ReaderT[Out, In, a]
    implicit val isoReaderT: Decoder <~> MT =
      new IsoFunctorTemplate[Decoder, MT] {
        def from[A](fa: MT[A]) = instance(fa.run(_))
        def to[A](fa: Decoder[A]) = ReaderT[Out, In, A](fa.decode)
      }

    implicit val monad: MonadError[Decoder, Int] = MonadError.fromIso(isoReaderT)
  }

  "fromIsoWithMonadError" in {
    Decoder[String].map(_.toUpperCase).decode("hello") must_=== "HELLO".right
  }

  "emap syntax" in {
    def f(s: String): Int \/ Char =
      if (s.length == 1) s(0).right
      else 1.left

    Decoder[String].emap(f).decode("hello") must_=== 1.left
    Decoder[String].emap(f).decode("g") must_=== 'g'.right
  }

}
