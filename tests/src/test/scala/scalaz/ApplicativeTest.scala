package scalaz

import scalacheck.{ScalazProperties, ScalazArbitrary}
import ScalazArbitrary._
import std.AllInstances._
import ScalazProperties.applicative

class ApplicativeTest extends Spec {

  checkAll("Validation", applicative.laws[({type λ[α]=Validation[Int, α]})#λ])
  checkAll("Zipper", applicative.laws[Zipper])
  checkAll("Option", applicative.laws[Option])

  implicit def zipperEqual[A: Equal]: Equal[Zipper[A]] = new Equal[Zipper[A]] {
    import std.stream.streamEqual
    def streamEqualApprox = streamEqual[A].contramap((_: Stream[A]).take(1000))
    def equal(a1: Zipper[A], a2: Zipper[A]) =
      streamEqualApprox.equal(a1.lefts, a2.lefts) &&
        Equal[A].equal(a1.focus, a2.focus) &&
        streamEqualApprox.equal(a1.rights, a2.rights)
  }
}
