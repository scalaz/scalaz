package scalaz.concurrent

import scalaz.{ Spec}
import scalaz.concurrent.Task._
import java.util.concurrent.{ThreadFactory, Executors}
import scala.collection.immutable.Queue
import scala.concurrent.SyncVar
import org.specs2.execute.{Success, Result}

/**
 *
 * User: pach
 * Date: 7/9/13
 * Time: 6:53 PM
 * (c) 2011-2013 Spinoco Czech Republic, a.s.
 */
object ConcurrentTaskTest extends Spec {

  "Task" should {

    "correctly use threads when forked and flatmapped" in {
      @volatile var q = Queue[(Int, String)]()

      val forked = "forked-thread"
      val current =  Thread.currentThread().getName

      def enqueue(taskId: Int) =
        q = q.enqueue((taskId, Thread.currentThread().getName))

      val es = Executors.newFixedThreadPool(1, new ThreadFactory {
        def newThread(p1: Runnable) = new Thread(p1, forked)
      })

      val sync = new SyncVar[Result]

      (for {
        _ <- now(enqueue(1))
        _ <- delay(enqueue(2))
        _ <- fork(now(enqueue(3)))(es)
        _ <- delay(enqueue(4))
        _ <- fork(now(enqueue(5)))(es)
        _ <- now(enqueue(6))
        _ <- fork(delay(enqueue(7)))(es)

      } yield ()).runAsync(_ => {
        enqueue(8)
        sync.set(Success())
      })
      enqueue(9)

      sync.get(5000).isDefined must_== true

      val runned = q.toList
      
      //trampoline should be evaluated at the head before anything else gets evaluated
      runned(0) must_== (1,current)
      runned(1) must_== (2,current)
      
      //the after async must not be the last ever
      runned.last._1 must_!=(9)
      
      //the rest of tasks must be run off the forked thread
      runned.filter(_._2 == forked).map(_._1) must_== List(3,4,5,6,7,8)
      
      
      
    }

  }

}
