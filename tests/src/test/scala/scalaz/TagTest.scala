package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

import Tags.{Multiplication => Mult}
import org.scalacheck.Prop.forAll

object TagTest extends SpecLite {
  "of.subst" should {
    "substitute" ! forAll {xs: List[Int] =>
      (Tag unwrap Foldable[List].fold(Tag.of[Mult].subst(xs))
         must_===(xs.foldLeft(1)(_ * _)))
    }
  }

  "of.onF" should {
    "substitute" ! forAll {xs: List[List[Unit]] =>
      (Tag unwrap (Foldable[List].foldMap(xs)(Tag.of[Mult].onF(_.length)))
         must_===(xs.foldLeft(1)((n, l) => n * l.length)))
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

  "of.unapply" should {
    "support unwrapping in pattern match" in {
      Monoid[Int @@ Mult].append(Mult(3), Mult(3)) match {
        case Mult(9) ⇒ true
        case Mult(_) ⇒ false
      }
    }
  }
}
