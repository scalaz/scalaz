package scalaz.example
package concurrent

import scalaz._
import scalaz.concurrent._

import java.util.concurrent.{TimeUnit, Executors}

object ExampleActor {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    sealed trait Event
    case class Login(user: String) extends Event
    case class Logout(user: String) extends Event
    case class ChatMessage(from: String, message: String) extends Event

    implicit val executor = Executors.newFixedThreadPool(5)
    import Strategy.Executor
    
    val chatServer = actor[Event] {
      (e: Event) => e match {
        case Login(user) => ("user: " + user + " logged in.").println
        case Logout(user) => ("user: " + user + " logged out.").println
        case ChatMessage(from, message) => ("user: " + from + " sent message: " + message).println
        case _ =>
      }
    }

    val user = "bob"
    chatServer ! Login(user)
    for (i <- 1 to 10) {
      chatServer ! ChatMessage(user, "SPAM: " ⊹ i.toString)
    }
    chatServer ! Logout(user)

    Thread.sleep(1000)
    executor.shutdown()
    executor.awaitTermination(60L, TimeUnit.SECONDS)
  }
}
