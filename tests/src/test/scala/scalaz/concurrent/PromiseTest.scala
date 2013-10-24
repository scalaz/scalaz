package scalaz
package concurrent

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop._
import ConcurrentTest._
import org.scalacheck.Prop.forAll

object PromiseTest extends SpecLite {
  implicit def promiseEqual[A: Equal] = Equal[A].contramap((_: Promise[A]).get)

  checkAll(monad.laws[Promise])
  checkAll(traverse.laws[Promise])
  checkAll(comonad.laws[Promise])

  "throw" ! (throws(Promise({throw new Error("x"); 1}).flatMap(_ => Promise(2)).get, classOf[Error]))
  "filter broken" ! (throws(Promise(0).filter(_ != 0).get, classOf[Promise.BrokenException]))
  "filter" ! (forAll((x: Int) => Promise(x).filter(_ => true).get must_=== x))

  class OhNo extends RuntimeException("OhNo!")

  "Promise" should {
    import Scalaz._
    "not hang when an error occurs in sequence" in {
      withTimeout(2000) {
        throws(List(Promise({ throw new OhNo(); 1 })).sequence.get, classOf[OhNo])
      }
    }
    "not hang when an error occurs on filter" in {
      withTimeout(2000) {
        throws(Promise({ throw new OhNo(); 2 }).filter(_ > 2).get, classOf[OhNo])
      }
    }
  }

}
