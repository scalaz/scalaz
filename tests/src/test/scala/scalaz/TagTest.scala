package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

import Tags.{Multiplication => Mult}

class TagTest extends Spec {
  "of.subst" should {
    "substitute" ! prop {xs: List[Int] =>
      (Foldable[List].fold(Tag.of[Mult].subst(xs))
         must be_===(xs.foldLeft(1)(_ * _)))
    }
  }

  "of.onF" should {
    "substitute" ! prop {xs: List[List[Unit]] =>
      (Foldable[List].foldMap(xs)(Tag.of[Mult].onF(_.length))
         must be_===(xs.foldLeft(1)((n, l) => n * l.length)))
    }
  }

  "of.onCov" should {
    "choose covariant position" in {
      Tag.of[Mult].onCov((a:Int) => a): (Int => (Int @@ Mult))
      true
    }
  }

  "of.onContra" should {
    "choose contravariant position" in {
      Tag.of[Mult].onContra(Show[Int]): Show[Int @@ Mult]
      true
    }
  }

  "of.unsubst" should {
    "substitute" in {
      Tag.of[Mult].unsubst(Semigroup[Int @@ Mult]): Semigroup[Int]
      true
    }
  }
}
