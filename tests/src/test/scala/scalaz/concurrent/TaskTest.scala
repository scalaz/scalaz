package scalaz
package concurrent

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.std.AllInstances._
import org.scalacheck.Prop._

class TaskTest extends Spec {
   
  val N = 10000
  val correct = (0 to N).sum
  val LM = Monad[List]; import LM.monadSyntax._; 
  val LT = Traverse[List]; import LT.traverseSyntax._

  // standard worst case scenario for trampolining - 
  // huge series of left associated binds
  def leftAssociatedBinds(seed: (=> Int) => Task[Int], 
                          cur: (=> Int) => Task[Int]): Task[Int] = 
    (0 to N).map(cur(_)).foldLeft(seed(0))(Task.taskInstance.lift2(_ + _))

  val options = List[(=> Int) => Task[Int]](n => Task.now(n), Task.delay _ , Task.fork _)
  val combinations = (options tuple options)

  "left associated binds" ! check {
    combinations.forall { case (seed, cur) => leftAssociatedBinds(seed, cur).run == correct }
  }

  "traverse-based map == sequential map" ! prop { (xs: List[Int]) => 
    xs.map(_ + 1) == xs.traverse(x => Task(x + 1)).run 
  }

  "gather-based map == sequential map" ! prop { (xs: List[Int]) => 
    xs.map(_ + 1) == Nondeterminism[Task].gather(xs.map(x => Task(x + 1))).run
  }

  case object FailWhale extends RuntimeException {
    override def fillInStackTrace = this 
  }

  "catches exceptions" ! check {
    Task.fork { Thread.sleep(10); throw FailWhale; 42 }.map(_ + 1).attemptRun == 
    Left(FailWhale)
  }

  "catches exceptions in antichain" ! prop { (x: Int, y: Int) => 
    val t1 = Task { Thread.sleep(1); throw FailWhale; 42 } 
    val t2 = Task { 43 } 
    Nondeterminism[Task].both(t1, t2).attemptRun == Left(FailWhale)
  }

}
