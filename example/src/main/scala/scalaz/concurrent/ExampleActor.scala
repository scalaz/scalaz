package scalaz
package concurrent

import java.util.concurrent.Executors

object ExampleActor {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    {
      sealed trait Event
      case class Login(user: String) extends Event
      case class Logout(user: String) extends Event
      case class ChatMessage(from: String, message: String) extends Event

      import strategy._
      implicit val strat = strategy.Executor.strategy[Unit](Executors.newFixedThreadPool(5))
      val chatServer = actor {
        (e: Event) => e match {
          case Login(user) => ("user: " + user + " logged in.").println
          case Logout(user) => ("user: " + user + " logged in.").println
          case ChatMessage(from, message) => {            
            ("user: " + from + " sent message: " + message).println
          }
          case _ => 
        }
      }
      chatServer
      val user = "bob"
      chatServer ! Login(user)
      for (i <- 1 to 10) {
        chatServer ! ChatMessage(user, "SPAM: " ⊹ i.toString)
      }
      chatServer ! Logout(user)
    }
  }
}