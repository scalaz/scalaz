package scalaz.concurrent

import scalaz.{Nondeterminism, Spec}
import java.util.concurrent.{Executors, ExecutorService}

/**
 *
 * User: pach
 * Date: 7/10/13
 * Time: 12:07 PM
 * (c) 2011-2013 Spinoco Czech Republic, a.s.
 */
object NondeterminismFutureTest extends Spec {
 

  
  "Future Nondeterminism" should {
    import scalaz.concurrent.Future._
    implicit val es = Executors.newFixedThreadPool(1)
    
    val non = Nondeterminism[Future]
   
    "correctly process gatherUnordered for >1 futures in non-blocking way" in {
      val f1 = fork(now(1))(es)
      val f2 = delay(7).flatMap(_=>fork(now(2))(es))
      val f3 = fork(now(3))(es)
      
      val f = fork(non.gatherUnordered(Seq(f1,f2,f3)))(es)
      
      f.run.toSet must_== Set(1,2,3)
    }


    "correctly process gatherUnordered for 1 future in non-blocking way" in {
      val f1 = fork(now(1))(es) 

      val f = fork(non.gatherUnordered(Seq(f1)))(es)

      f.run.toSet must_== Set(1)
    }

    "correctly process gatherUnordered for empty seq of futures in non-blocking way" in {
      val f = fork(non.gatherUnordered(Seq()))(es)

      f.run.toSet must_== Set()
    }



  }
  
}
