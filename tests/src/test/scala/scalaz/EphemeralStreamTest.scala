package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import syntax.contravariant._

class EphemeralStreamTest extends Spec {

  checkAll(equal.laws[EphemeralStream[Int]])
  checkAll(monadPlus.laws[EphemeralStream])
  checkAll(traverse.laws[EphemeralStream])

  implicit def ephemeralStreamShow[A: Show]: Show[EphemeralStream[A]] =
    Show[List[A]].contramap(_.toList)

  "reverse" ! prop{ e: EphemeralStream[Int] =>
    e.reverse.toList must be_===(e.toList.reverse)
    e.reverse.reverse must be_===(e)
  }
}
