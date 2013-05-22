package scalaz
package concurrent

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.specs2.execute.{Failure, Result}
import ConcurrentTest._
import scalaz.syntax.either._

class TimerTest extends Spec {
  def withTimer[T](expression: Timer => T): T = {
    val timer = new Timer(10)
    try {
      expression(timer)
    } finally {
      timer.stop()
    }
  }
  "Timer" should {
    "stop normally" in {
      withTimer(timer => ())
    }
    "handle stop being called repeatedly" in {
      withTimer{timer =>
        timer.stop()
      }
    }
    "valueWait produces a value after the specified timeout" in {
      withTimer{timer =>
        val start = System.currentTimeMillis
        withTimeout(5000){
          val future = timer.valueWait("Test", 100)
          future.run must_== "Test"
          (System.currentTimeMillis - start) >= 100
        }
      }
    }
    "withTimeout(Future...) produces a Timeout if the timeout is exceeded" in {
      withTimer{timer =>
        val future = timer.withTimeout(Future{Thread.sleep(500); "Test"}, 100)
        withTimeout(5000){
          future.run must_== Timeout.left
        }
      }
    }
    "produces the result of the Future if the timeout is not exceeded" in {
      withTimer{timer =>
        val future = timer.withTimeout(Future{Thread.sleep(50); "Test"}, 200)
        withTimeout(5000){
          future.run must_== "Test".right
        }
      }
    }
  }
}
