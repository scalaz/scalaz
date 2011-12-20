package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class WriterTTest extends Spec {

  type WriterTOpt[W, A] = WriterT[Option, W, A]
  type WriterTOptInt[A] = WriterTOpt[Int, A]

  checkAll(equal.laws[WriterTOptInt[Int]])
  checkAll(monad.laws[WriterTOptInt])
  checkAll(traverse.laws[WriterTOptInt])
  checkAll(bifunctor.laws[WriterTOpt])
}
