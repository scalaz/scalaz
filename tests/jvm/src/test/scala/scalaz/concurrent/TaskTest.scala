package scalaz
package concurrent

import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop._

import java.util.concurrent.{Executors, TimeoutException, TimeUnit}
import java.util.concurrent.atomic._
import org.scalacheck.Prop.forAll

object TaskTest extends SpecLite {

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
    combinations.forall { case (seed, cur) => leftAssociatedBinds(seed, cur).unsafePerformSync == correct }
  }

  "traverse-based map == sequential map" ! forAll { (xs: List[Int]) =>
    xs.map(_ + 1) == xs.traverse(x => Task(x + 1)).unsafePerformSync
  }

  "gather-based map == sequential map" ! forAll { (xs: List[Int]) =>
    xs.map(_ + 1) == Nondeterminism[Task].gather(xs.map(x => Task(x + 1))).unsafePerformSync
  }

  case object FailWhale extends RuntimeException {
    override def fillInStackTrace = this
  }

  case object SadTrombone extends RuntimeException {
    override def fillInStackTrace = this
  }

  case object FailTurkey extends Error {
    override def fillInStackTrace = this
  }

  "catches exceptions" ! {
    Task { Thread.sleep(10); throw FailWhale; 42 }.map(_ + 1).unsafePerformSyncAttempt ==
    -\/(FailWhale)
  }

  "catches errors" ! {
    Task { Thread.sleep(10); throw FailTurkey; 42 }.map(_ + 1).unsafePerformSyncAttempt ==
    -\/(FailTurkey)
  }

  "catches exceptions in a mapped function" ! {
    Task { Thread.sleep(10); 42 }.map(_ => throw FailWhale).unsafePerformSyncAttempt ==
    -\/(FailWhale)
  }

  "catches exceptions in a mapped function, created by delay" ! {
    Task.delay { Thread.sleep(10); 42 }.map(_ => throw FailWhale).unsafePerformSyncAttempt ==
    -\/(FailWhale)
  }

  "catches exceptions in a mapped function, created with now" ! {
    Task.now { Thread.sleep(10); 42 }.map(_ => throw FailWhale).unsafePerformSyncAttempt ==
    -\/(FailWhale)
  }

  "catches exceptions in a flatMapped function" ! {
    Task { Thread.sleep(10); 42 }.flatMap(_ => throw FailWhale).unsafePerformSyncAttempt ==
    -\/(FailWhale)
  }

  "catches exceptions in a flatMapped function, created with delay" ! {
    Task.delay { Thread.sleep(10); 42 }.flatMap(_ => throw FailWhale).unsafePerformSyncAttempt ==
    -\/(FailWhale)
  }

  "catches exceptions in a flatMapped function, created with now" ! {
    Task.now { Thread.sleep(10); 42 }.flatMap(_ => throw FailWhale).unsafePerformSyncAttempt ==
    -\/(FailWhale)
  }

  "catches exceptions in parallel execution" ! forAll { (x: Int, y: Int) =>
    val t1 = Task { Thread.sleep(10); throw FailWhale; 42 }
    val t2 = Task { 43 }
    Nondeterminism[Task].both(t1, t2).unsafePerformSyncAttempt == -\/(FailWhale)
  }

  "handles exceptions in handle" ! {
    Task { Thread.sleep(10); throw FailWhale; 42 }.handle { case FailWhale => 84 }.unsafePerformSyncAttempt ==
      \/-(84)
  }

  "leaves unhandled exceptions alone in handle" ! {
    Task { Thread.sleep(10); throw FailWhale; 42 }.handle { case SadTrombone => 84 }.unsafePerformSyncAttempt ==
      -\/(FailWhale)
  }

  "catches exceptions thrown in handle" ! {
    Task { Thread.sleep(10); throw FailWhale; 42 }.handle { case FailWhale => throw SadTrombone }.unsafePerformSyncAttempt ==
      -\/(SadTrombone)
  }

  "handles exceptions in handleWith" ! {
    val foo =
    Task { Thread.sleep(10); throw FailWhale; 42 }.handleWith { case FailWhale => Task.delay(84) }.unsafePerformSyncAttempt ==
      \/-(84)
  }

  "leaves unhandled exceptions alone in handleWith" ! {
    Task { Thread.sleep(10); throw FailWhale; 42 }.handleWith { case SadTrombone => Task.delay(84) }.unsafePerformSyncAttempt ==
      -\/(FailWhale)
  }

  "catches exceptions thrown in handleWith" ! {
    Task { Thread.sleep(10); throw FailWhale; 42 }.handleWith { case FailWhale => Task.delay(throw SadTrombone) }.unsafePerformSyncAttempt ==
      -\/(SadTrombone)
  }

  "catches exceptions thrown by onFinish argument function" ! {
    Task { Thread.sleep(10); 42 }.onFinish { _ => throw SadTrombone; Task.now(()) }.unsafePerformSyncAttemptFor(1000) ==
      -\/(SadTrombone)
  }  
  
  "evaluates Monad[Task].point lazily" in {
    val M = implicitly[Monad[Task]]
    var x = 0
    M point { x += 1 }
    x must_== 0
  }


  "Nondeterminism[Task]" should {
    import scalaz.concurrent.Task._
    val es = Executors.newFixedThreadPool(1)
    val intSetReducer = Reducer.unitReducer[Int, Set[Int]](Set(_))

    "correctly process reduceUnordered for >1 tasks in non-blocking way" in {
      val t1 = fork(now(1))(es)
      val t2 = delay(7).flatMap(_=>fork(now(2))(es))
      val t3 = fork(now(3))(es)
      val t = fork(Task.reduceUnordered(Seq(t1,t2,t3))(intSetReducer))(es)

      t.unsafePerformSync must_== Set(1,2,3)
    }


    "correctly process reduceUnordered for 1 task in non-blocking way" in {
      val t1 = fork(now(1))(es)

      val t = fork(Task.reduceUnordered(Seq(t1))(intSetReducer))(es)

      t.unsafePerformSync must_== Set(1)
    }

    "correctly process reduceUnordered for empty seq of tasks in non-blocking way" in {
      val t = fork(Task.reduceUnordered(Seq())(intSetReducer))(es)

      t.unsafePerformSync must_== Set()
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

      t.unsafePerformSyncAttempt mustMatch {
        case -\/(e) => e must_== ex; true
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

      t.unsafePerformSyncAttempt mustMatch {
        case -\/(e) => e must_== ex; true
      }

      sleep(3000)

      t1v.get must_== 0
      t3v.get must_== 0
    }

    "nmap6 must run Tasks in parallel" in {
      import Thread._
      import java.{util => ju}
      import ju.concurrent.CyclicBarrier

      //Ensure at least 6 different threads are available.
      implicit val es6 =
        Executors.newFixedThreadPool(6)
      val barrier = new CyclicBarrier(6);

      val seenThreadNames = scala.collection.JavaConversions.asScalaSet(ju.Collections.synchronizedSet(new ju.HashSet[String]()))
      val t =
        for (i <- 0 to 5) yield fork {
          seenThreadNames += currentThread().getName()
          //Prevent the execution scheduler from reusing threads. This will only
          //proceed after all 6 threads reached this point.
          barrier.await(1, TimeUnit.SECONDS)
          now(('a' + i).toChar)
        }

      val r = Nondeterminism[Task].nmap6(t(0), t(1), t(2), t(3), t(4), t(5))(List(_,_,_,_,_,_))
      val chars = List('a','b','c','d','e','f')
      r.unsafePerformSync must_== chars
      //Ensure we saw 6 distinct threads.
      seenThreadNames.size must_== 6
    }

    "correctly exit when timeout is exceeded on runFor" in {

      val es = Executors.newFixedThreadPool(1)

      val t =  fork { Thread.sleep(3000); now(1) }(es)

       t.unsafePerformSyncAttemptFor(100) mustMatch {
         case -\/(ex:TimeoutException) => true
       }

      es.shutdown()
    }

    "correctly cancels scheduling of all tasks once first task hit timeout" in {
      val es = Executors.newFixedThreadPool(1)

      @volatile var bool = false

      val t =  fork { Thread.sleep(1000); now(1) }(es).map(_=> bool = true)

      t.unsafePerformSyncAttemptFor(100) mustMatch {
        case -\/(ex:TimeoutException) => true
      }

      Thread.sleep(1500)

      bool must_== false

      es.shutdown()
    }
  }

  "retries a retriable task n times" ! forAll { xs: List[Byte] =>
    import scala.concurrent.duration._
    var x = 0
    Task.delay {x += 1; sys.error("oops")}.retry(xs.map(_ => 0.milliseconds)).attempt.unsafePerformSync
    x == (xs.length + 1)
  }

  "fromMaybe empty fails" ! forAll { t: Throwable =>
    Task.fromMaybe(Maybe.empty)(t).unsafePerformSyncAttempt.isLeft
  }

  "fromMaybe just succeeds" ! forAll { (n: Int, t: Throwable) =>
    Task.fromMaybe(Maybe.just(n))(t).unsafePerformSyncAttempt.isRight
  }

  "fromDisjunction matches attemptRun" ! forAll { x: Throwable \/ Int =>
    Task.fromDisjunction(x).unsafePerformSyncAttempt must_== x
  }
}

