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

    // type ascriptions are needed for type inference
    type I = String
    type O[a] = Int \/ a
    type MT[a] = Kleisli[O, I, a]
    val iso: Decoder <~> MT = Kleisli.iso[Decoder, I, O](
      instance = λ[λ[a => (I => O[a])] ~> Decoder](instance(_)),
      decode   = λ[Decoder ~> λ[a => (I => O[a])]](_.decode)
    )
    // with `-Ypartial-unification` we could write
    //
    // val iso: Decoder <~> Kleisli[Int \/ *, String, *] = Kleisli.iso(
    //   λ[λ[a => (String => Int \/ a)] ~> Decoder](instance(_)),
    //   λ[Decoder ~> λ[a => (String => Int \/ a)]](_.decode)
    // )
    //
    // or introduce `type Decode[a] = String => Int \/ a` to keep it terse.

    implicit val monad: MonadError[Decoder, Int] = MonadError.fromIso(iso)
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
