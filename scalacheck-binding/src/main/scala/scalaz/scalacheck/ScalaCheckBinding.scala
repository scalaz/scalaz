package scalaz
package scalacheck

object ScalaCheckBinding extends ScalaCheckBindings

/**
 * Type class instances for types from <a href="http://code.google.com/p/scalacheck">Scalacheck</a>
 */
trait ScalaCheckBindings {
  import data.*._
  import Semigroup._
  import org.scalacheck.{Gen, Arbitrary}
  import Gen.{sized, value}

  implicit val GenBind: Bind[Gen] = new Bind[Gen] {
    def bind[A, B](f: A => Gen[B]) =
      _ flatMap f
  }

  implicit val ArbitraryBind: Bind[Arbitrary] = new Bind[Arbitrary] {
    def bind[A, B](f: A => Arbitrary[B]) =
      r => Arbitrary(r.arbitrary flatMap (f(_).arbitrary))
  }

  implicit val GenFunctor: Functor[Gen] = new Functor[Gen] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit val ArbitraryFunctor: Functor[Arbitrary] = new Functor[Arbitrary] {
    def fmap[A, B](f: A => B) =
      r => Arbitrary(r.arbitrary map f)
  }

  implicit val GenPointed: Pointed[Gen] = new Pointed[Gen] {
    def point[A](a: => A): Gen[A] =
      sized(_ => value(a))
  }

  implicit val ArbitraryPointed: Pointed[Arbitrary] = new Pointed[Arbitrary] {
    def point[A](a: => A) = Arbitrary(sized(_ => value(a)))
  }

  implicit def GenSemigroup[A](implicit s: Semigroup[A]) =
    semigroup[Gen[A]](a => b => Gen(p => a(p) ⊹ (b apply p)))

  implicit def ArbitrarySemigroup[A](implicit s: Semigroup[A]) =
    semigroup[Arbitrary[A]](a => b => Arbitrary(a.arbitrary ⊹ b.arbitrary))

  implicit def GenZero[A](implicit z: Zero[A]): Gen[A] = Gen(_ => Some(z.zero))

  implicit def ArbitraryZero[A]: Arbitrary[A] = Arbitrary(Gen(_ => None: Option[A]))

}