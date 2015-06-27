package scalaz
package std

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.std.AllInstances._
import scalaz.syntax.traverse._
import scalaz.Tags._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class FutureTest extends SpecLite {

  val duration: Duration = 1.seconds

  implicit def futureEqual[A : Equal] = Equal[A] contramap { future: Future[A] => Await.result(future, duration) }

  implicit def FutureArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[Future[A]] = implicitly[Arbitrary[A]] map { x => Future(x) }

  checkAll(monad.laws[Future])
  checkAll(monoid.laws[Future[Int]])
  checkAll(monoid.laws[Future[Int @@ Multiplication]])

  // Scope these away from the rest as Comonad[Future] is a little evil.
  // Should fail to compile by default: implicitly[Comonad[Future]]
  {
    implicit val cm: Comonad[Future] = futureComonad(duration)
    checkAll(comonad.laws[Future])
  }

  "Apply[Future]" should {
    "not SOE on traverse of big Stream" in {
      val f = (1 to 100000).toStream.traverse { i => Promise.successful(i).future }
      try {
        Await.result(f, 10.seconds)
        true
      } catch {
        case (soe: StackOverflowError) =>
          fail("stack overflow")
      }
    }
  }
}
