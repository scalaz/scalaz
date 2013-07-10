package scalaz.concurrent

import java.util.concurrent.Executors
import scalaz.{-\/, Spec}
import java.util.concurrent.atomic.AtomicInteger

/**
 *
 * User: pach
 * Date: 7/10/13
 * Time: 2:58 PM
 * (c) 2011-2013 Spinoco Czech Republic, a.s.
 */
object NondeterminismTaskTest extends Spec{

  "Future Nondeterminism" should {
    import scalaz.concurrent.Task._
    val es = Executors.newFixedThreadPool(1)


    "correctly process gatherUnordered for >1 futures in non-blocking way" in {
      val t1 = fork(now(1))(es)
      val t2 = delay(7).flatMap(_=>fork(now(2))(es))
      val t3 = fork(now(3))(es)
      val t = fork(Task.gatherUnordered(Seq(t1,t2,t3)))(es)

      t.run.toSet must_== Set(1,2,3)
    }


    "correctly process gatherUnordered for 1 future in non-blocking way" in {
      val t1 = fork(now(1))(es)

      val t = fork(Task.gatherUnordered(Seq(t1)))(es)

      t.run.toSet must_== Set(1)
    }

    "correctly process gatherUnordered for empty seq of futures in non-blocking way" in {
      val t = fork(Task.gatherUnordered(Seq()))(es)

      t.run.toSet must_== Set()
    }

    "early terminate once any of the futures failed" in {
      import Thread._
      val ex = new Exception("expected")
      
      val t1v = new AtomicInteger(0)
      val t3v = new AtomicInteger(0)

      val es3 = Executors.newFixedThreadPool(3)
      
      
      val t1 = fork{sleep(1000); t1v.set(1);now(1)}(es3)
      val t2 = fork(now(throw ex))(es3)
      val t3 = fork{sleep(1000); t3v.set(3); now(3)}(es3)

      val t = fork(Task.gatherUnordered(Seq(t1,t2,t3)))(es3)
      
      t.attemptRun match {
        case -\/(e) => e must_== ex 
      }
      
      t1v.get must_== 0
      t3v.get must_== 0
      
    }


    "early terminate once any of the futures failed, and cancels execution" in {
      import Thread._
      val ex = new Exception("expected")

      val t1v = new AtomicInteger(0)
      val t3v = new AtomicInteger(0)

      val es3 = Executors.newFixedThreadPool(3)


      val t1 = fork{sleep(1000); t1v.set(1);now(1)}(es3).flatMap(_=>fork{t1v.set(10);now(10)}(es3)) 
      val t2 = fork{sleep(100); now(throw ex)}(es3)
      val t3 = fork{sleep(1000); t3v.set(3); now(3)}(es3).flatMap(_=>fork{t1v.set(30);now(30)}(es3))

      val t = fork(Task.gatherUnordered(Seq(t1,t2,t3), cancelOnEarlyExit = true))(es3)

      t.attemptRun match {
        case -\/(e) => e must_== ex 
      }
      
      sleep(3000)

      t1v.get must_== 1
      t3v.get must_== 3

    }
    
    

  }


}
