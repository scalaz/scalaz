package scalaz.example.concurrent

import scalaz._
import Scalaz._

import concurrent._
import java.util.concurrent.Executors._
import java.util.concurrent.ExecutorService

object HammerTime {
  def main(args: Array[String]): Unit = run

  def run {
    implicit val pool = newCachedThreadPool
    implicit val strategy = Strategy.Executor

    val done = actor((u: Unit) => {
      println("TEST SUCCEEDED")
      pool.shutdown
    })

    val fail = (e: Throwable) => {
      e.printStackTrace
      pool.shutdown
    }

    def hammer(other: => Actor[Int]) = actor(fail, (i: Int) =>
      if (i == 0) done ! ()
      else other ! (i - 1)
      )

    lazy val hammer1: Actor[Int] = hammer(hammer(hammer1))
    hammer1 ! 1000000
  }
}
