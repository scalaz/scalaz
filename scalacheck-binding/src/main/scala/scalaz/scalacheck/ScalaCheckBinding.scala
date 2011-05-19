package scalaz
package scalacheck

import scalaz.Zero

object ScalaCheckBinding extends ScalaCheckBindings

/**
 * Type class instances for types from <a href="http://code.google.com/p/scalacheck">Scalacheck</a>
 */
trait ScalaCheckBindings {
  import data.*._
  import Semigroup._
  import org.scalacheck.{Gen, Arbitrary}
  import Gen.{sized, value}

  implicit def GenSemigroup[A](implicit s: Semigroup[A]) =
    semigroup[Gen[A]](a => b => Gen(p => a(p) ⊹ (b apply p)))

  implicit def GenZero[A](implicit z: Zero[A]): Gen[A] = Gen(_ => Some(z.zero))

  implicit val GenFunctor: Functor[Gen] = new Functor[Gen] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit val GenApplic: Applic[Gen] = new Applic[Gen] {
    def applic[A, B](f: Gen[A => B]) =
      a => f flatMap (k => a map k)
  }

  implicit val GenApplicFunctor: ApplicFunctor[Gen] =
    ApplicFunctor.applicFunctor[Gen]

  implicit val GenPointed: Pointed[Gen] = new Pointed[Gen] {
    def point[A](a: => A): Gen[A] =
      sized(_ => value(a))
  }

  implicit val GenPointedFunctor: PointedFunctor[Gen] =
    PointedFunctor.pointedFunctor[Gen]

  implicit val GenApplicative: Applicative[Gen] =
    Applicative.applicative[Gen]

  implicit val GenBind: Bind[Gen] = new Bind[Gen] {
    def bind[A, B](f: A => Gen[B]) =
      _ flatMap f
  }

  implicit val GenBindFunctor: BindFunctor[Gen] =
    BindFunctor.bindFunctor

  implicit val GenJoin: Join[Gen] = new Join[Gen] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit val GenMonad: Monad[Gen] =
    Monad.monadBP[Gen]

  implicit def ArbitrarySemigroup[A](implicit s: Semigroup[A]): Semigroup[Arbitrary[A]] =
    semigroup[Arbitrary[A]](a => b => Arbitrary(a.arbitrary ⊹ b.arbitrary))

  implicit def ArbitraryZero[A]: Zero[Arbitrary[A]] =
    Zero.zero(Arbitrary((Gen(_ => None: Option[A]))))

  implicit val ArbitraryFunctor: Functor[Arbitrary] = new Functor[Arbitrary] {
    def fmap[A, B](f: A => B) =
      r => Arbitrary(r.arbitrary map f)
  }

  implicit val ArbitraryApplic: Applic[Arbitrary] = new Applic[Arbitrary] {
    def applic[A, B](f: Arbitrary[A => B]) =
      a => Arbitrary(f.arbitrary flatMap (k => a.arbitrary map k))
  }

  implicit val ArbitraryApplicFunctor: ApplicFunctor[Arbitrary] =
    ApplicFunctor.applicFunctor[Arbitrary]

  implicit val ArbitraryPointed: Pointed[Arbitrary] = new Pointed[Arbitrary] {
    def point[A](a: => A) = Arbitrary(sized(_ => value(a)))
  }

  implicit val ArbitraryPointedFunctor: PointedFunctor[Arbitrary] =
    PointedFunctor.pointedFunctor[Arbitrary]

  implicit val ArbitraryApplicative: Applicative[Arbitrary] =
    Applicative.applicative[Arbitrary]

  implicit val ArbitraryBind: Bind[Arbitrary] = new Bind[Arbitrary] {
    def bind[A, B](f: A => Arbitrary[B]) =
      r => Arbitrary(r.arbitrary flatMap (f(_).arbitrary))
  }

  implicit val ArbitraryBindFunctor: BindFunctor[Arbitrary] =
    BindFunctor.bindFunctor

  implicit val ArbitraryJoin: Join[Arbitrary] = new Join[Arbitrary] {
    def join[A] =
      a => Arbitrary(a.arbitrary flatMap (z => z.arbitrary))
  }

  implicit val ArbitraryMonad: Monad[Arbitrary] =
    Monad.monadBP[Arbitrary]

  implicit def ArbitraryUnpack[A]: Unpack[Arbitrary[A], Gen[A]] = new Unpack[Arbitrary[A], Gen[A]] {
    val unpack = (_: Arbitrary[A]).arbitrary
  }

  implicit def ArbitraryPack[A]: Pack[Arbitrary[A], Gen[A]] = new Pack[Arbitrary[A], Gen[A]] {
    val pack = (b: Gen[A]) => Arbitrary(b)
  }

  implicit def ArbitraryNewtype[A]: Newtype[Arbitrary[A], Gen[A]] =
    Newtype.newtype

}