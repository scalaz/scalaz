package scalaz
package std

import _root_.java.util.concurrent.Executors

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.std.AllInstances._
import scalaz.syntax.functor._
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

  // Scope these away from the rest as Copointed[Future] is a little evil.
  // Should fail to compile by default: implicitly[Copointed[Future]]
  {
    implicit val cm: Comonad[Future] = futureComonad(duration)
    checkAll(comonad.laws[Future])
  }

  "Nondeterminism[Future]" should {
    implicit val es: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1))

    "fetch first completed future in chooseAny" ! forAll { (xs: Vector[Int]) =>
      val promises = Vector.fill(xs.size)(Promise[Int]())
      def loop(is: List[Int], fs: Seq[Future[Int]], acc: Vector[Int]): Future[Vector[Int]] =
        is match {
          case i :: is0 =>
            promises(i).complete(scala.util.Try(xs(i)))
            Nondeterminism[Future].chooseAny(fs).get.flatMap { case (x, fs0) =>
              loop(is0, fs0, acc :+ x)
            }
          case Nil =>
            Future(acc)
        }

      val sorted = xs.zipWithIndex.sorted
      val sortedF = loop(sorted.map(_._2).toList, promises.map(_.future), Vector.empty)
      Await.result(sortedF, duration) must_== sorted.map(_._1)
    }

    "gather maintains order" ! forAll { (xs: List[Int]) =>
      val promises = Vector.fill(xs.size)(Promise[Int]())
      val f = Nondeterminism[Future].gather(promises.map(_.future))
      (promises zip xs).reverseIterator.foreach { case (p, x) =>
        p.complete(scala.util.Try(x))
      }
      Await.result(f, duration) must_== xs
    }
  }
}
