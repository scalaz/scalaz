package scalaz

import org.scalacheck.{Gen, Arbitrary}
import Scalaz._
import scalaz.Semigroup._

trait SemigroupImplicits {
  implicit def GenSemigroup[A](implicit s: Semigroup[A]) = semigroup[Gen[A]]((a, b) => Gen(p => a(p) |+| (b apply p)))

  implicit def ArbitrarySemigroup[A](implicit s: Semigroup[A]) = semigroup[Arbitrary[A]]((a, b) => Arbitrary(a.arbitrary |+| b.arbitrary))
}
