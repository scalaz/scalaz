package scalaz
package concurrent

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.std.AllInstances._
import org.scalacheck.Prop._

import java.util.concurrent.{Executors, TimeoutException}
import java.util.concurrent.atomic._


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

  val options = List[(=> Int) => Task[Int]](n => Task.now(n), Task.delay _ , Task.apply _)
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
    Task { Thread.sleep(10); throw FailWhale; 42 }.map(_ + 1).attemptRun ==
    -\/(FailWhale)
  }

  "catches exceptions in parallel execution" ! prop { (x: Int, y: Int) => 
    val t1 = Task { Thread.sleep(10); throw FailWhale; 42 } 
    val t2 = Task { 43 } 
    Nondeterminism[Task].both(t1, t2).attemptRun == -\/(FailWhale)
  }

  "Nondeterminism[Task]" should {
    import scalaz.concurrent.Task._
    val es = Executors.newFixedThreadPool(1)


    "correctly process gatherUnordered for >1 tasks in non-blocking way" in {
      val t1 = fork(now(1))(es)
      val t2 = delay(7).flatMap(_=>fork(now(2))(es))
      val t3 = fork(now(3))(es)
      val t = fork(Task.gatherUnordered(Seq(t1,t2,t3)))(es)

      t.run.toSet must_== Set(1,2,3)
    }


    "correctly process gatherUnordered for 1 task in non-blocking way" in {
      val t1 = fork(now(1))(es)

      val t = fork(Task.gatherUnordered(Seq(t1)))(es)

      t.run.toSet must_== Set(1)
    }

    "correctly process gatherUnordered for empty seq of tasks in non-blocking way" in {
      val t = fork(Task.gatherUnordered(Seq()))(es)

      t.run.toSet must_== Set()
    }

    "early terminate once any of the tasks failed" in {
      import Thread._
      val ex = new RuntimeException("expected")
      
      val t1v = new AtomicInteger(0)
      val t3v = new AtomicInteger(0)

      val es3 = Executors.newFixedThreadPool(3)
      
      // NB: Task can only be interrupted in between steps (before the `map`)
      val t1 = fork { sleep(1000); now(()) }.map { _ => t1v.set(1) }
      val t2 = fork { now(throw ex) }
      val t3 = fork { sleep(1000); now(()) }.map { _ => t3v.set(3) }

      val t = fork(Task.gatherUnordered(Seq(t1,t2,t3), exceptionCancels = true))(es3)
      
      t.attemptRun match {
        case -\/(e) => e must_== ex 
      }
      
      t1v.get must_== 0
      t3v.get must_== 0
    }

    "early terminate once any of the tasks failed, and cancels execution" in {
      import Thread._
      val ex = new RuntimeException("expected")

      val t1v = new AtomicInteger(0)
      val t3v = new AtomicInteger(0)

      implicit val es3 = Executors.newFixedThreadPool(3)

      // NB: Task can only be interrupted in between steps (before the `map`)
      val t1 = fork { sleep(1000); now(()) }.map { _ => t1v.set(1) }
      val t2 = fork { sleep(100); now(throw ex) }
      val t3 = fork { sleep(1000); now(()) }.map { _ => t3v.set(3) }

      val t = fork(Task.gatherUnordered(Seq(t1,t2,t3), exceptionCancels = true))(es3)

      t.attemptRun match {
        case -\/(e) => e must_== ex 
      }
      
      sleep(3000)

      t1v.get must_== 0
      t3v.get must_== 0
    }


    "correctly exit when timeout is exceeded on runFor" in {

      val es = Executors.newFixedThreadPool(1)
      
      val t =  fork { Thread.sleep(3000); now(1) }(es)
      
       t.attemptRunFor(100) match {
         case -\/(ex:TimeoutException)  => //ok
       } 
   
      es.shutdown()
    }
    
    "correctly cancels scheduling of all tasks once first task hit timeout" in {
      val es = Executors.newFixedThreadPool(1)

      @volatile var bool = false
      
      val t =  fork { Thread.sleep(1000); now(1) }(es).map(_=> bool = true)

      t.attemptRunFor(100) match {
        case -\/(ex:TimeoutException)  => //ok
      }

      Thread.sleep(1500)

      bool must_== false

      es.shutdown()
    }
  }
}

