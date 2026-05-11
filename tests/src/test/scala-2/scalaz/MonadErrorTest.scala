package scalaz

import std.AllInstances._
import syntax.either._
import syntax.monadError._
import Isomorphism.<~>

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

    val iso: Decoder <~> Kleisli[\/[Int, *], String, *] = Kleisli.iso(
      new (λ[a => (String => Int \/ a)] ~> Decoder){
        override def apply[A](a: String => (Int \/ A)) =
          instance(a)
      },
      new (Decoder ~> λ[a => (String => Int \/ a)]) {
        override def apply[A](a: Decoder[A]) =
          a decode _
      }
    )

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
