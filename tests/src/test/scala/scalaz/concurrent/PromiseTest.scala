package scalaz
package concurrent

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop._

class PromiseTest extends Spec {
  implicit def promiseEqual[A: Equal] = Equal[A].contramap((_: Promise[A]).get)

  checkAll(monad.laws[Promise])
  checkAll(traverse.laws[Promise])
  checkAll(comonad.laws[Promise])

  check(throws(Promise({throw new Error("x"); 1}).flatMap(_ => Promise(2)).get, classOf[Error]))
  check(throws(Promise(0).filter(_ != 0).get, classOf[Promise.BrokenException]))
  check(forAll((x: Int) => Promise(x).filter(_ => true).get === x))

}
