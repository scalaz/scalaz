package scalaz
package concurrent

import java.util.concurrent._

/**
 * Evaluate an expression in some specific manner. A typical strategy will schedule asynchronous
 * evaluation and return a function that, when called, will block until the result is ready.
 *
 * Memory consistency effects: Actions in a thread prior to the submission of `a`
 * to the `Strategy` happen-before any actions taken by `a`, which in turn happen-before
 * the result is retrieved via returned function.
 */
trait Strategy {
  def apply[A](a: => A): () => A

  /**
   * Number of messages that will be handled in batch by actors.
   */
  private[concurrent] def batch: Long = 10
}

object Strategy extends Strategys

trait Strategys extends StrategysLow {

  /**
   * Default thread factory to mark all threads as daemon
   */
  lazy val DefaultDaemonThreadFactory = new ThreadFactory {
    val defaultThreadFactory = Executors.defaultThreadFactory()

    def newThread(r: Runnable) = {
      val t = defaultThreadFactory.newThread(r)
      t.setDaemon(true)
      t
    }
  }

  /**
   * The default executor service is a fixed thread pool with N daemon threads,
   * where N is equal to the number of available processors.
   */
  lazy val DefaultExecutorService: ExecutorService =
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors, DefaultDaemonThreadFactory)

  /**
   * Default fork-join pool with LIFO scheduling mode for forked tasks,
   * where parallelism is equal to the number of available processors.
   * This pool is suitable for parallel algorithms where tasks are joined.
   */
  lazy val DefaultLIFOForkJoinPool: ForkJoinPool =
    new ForkJoinPool(Runtime.getRuntime.availableProcessors, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true)

  /**
   * Default fork-join pool with FIFO scheduling mode for forked tasks,
   * where parallelism is equal to the number of available processors.
   * This pool is suitable for actors and other message-passing systems.
   */
  lazy val DefaultFIFOForkJoinPool: ForkJoinPool =
    new ForkJoinPool(Runtime.getRuntime.availableProcessors, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, true)

  /**
   * Default scheduler used for scheduling the tasks like timeout.
   */
  lazy val DefaultTimeoutScheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(1,
    DefaultDaemonThreadFactory)

  /**
   * A strategy that executes its arguments on `DefaultExecutorService`
   */
  implicit lazy val DefaultStrategy: Strategy = Executor(DefaultExecutorService)
}

trait StrategysLow {

  /**
   * A strategy that evaluates its argument in the current thread.
   */
  implicit val Sequential: Strategy = new Strategy {
    def apply[A](a: => A) = {
      val v = a
      () => v
    }

    private[concurrent] override val batch: Long = -1 // maximize batch size to avoid stack overflow in actors
  }

  /**
   * A strategy that evaluates its arguments using an implicit ExecutorService.
   */
  implicit def Executor(implicit s: ExecutorService): Strategy =
    Executor(execService = s, batchSize = 10)

  /**
   * A strategy that evaluates its arguments using an ExecutorService.
   *
   * Implementation of tasks for FJ pools is based on improvements committed to Akka by Viktor Klang:
   * https://github.com/akka/akka/pull/16152
   *
   * @param execService Executor service that will run strategy
   * @param batchSize   Number of messages that actor will handle in batch
   *                    before next actor will handle own messages in the same thread,
   *                    set to 1 for as fair as possible
   */
  def Executor(execService: ExecutorService, batchSize: Long): Strategy = execService match {
    case pool: scala.concurrent.forkjoin.ForkJoinPool => new Strategy {

      import scala.concurrent.forkjoin.ForkJoinTask

      def apply[A](a: => A) = {
        val task = new ForkJoinTask[A] {
          private var r: A = _

          def getRawResult: A = r

          def setRawResult(a: A): Unit = r = a

          def exec(): Boolean = {
            r = a
            true
          }
        }
        if (ForkJoinTask.getPool eq pool) task.fork()
        else pool.execute(task)
        () => task.get
      }

      private[concurrent] override val batch: Long = batchSize
    }
    case pool: ForkJoinPool => new Strategy {
      def apply[A](a: => A) = {
        val task = new ForkJoinTask[A] {
          private var r: A = _

          def getRawResult: A = r

          def setRawResult(a: A): Unit = r = a

          def exec(): Boolean = {
            r = a
            true
          }
        }
        if (ForkJoinTask.getPool eq pool) task.fork()
        else pool.execute(task)
        () => task.get
      }

      private[concurrent] override val batch: Long = batchSize
    }
    case pool => new Strategy {
      def apply[A](a: => A) = {
        val future = pool.submit(new Callable[A] {
          def call: A = a
        })
        () => future.get
      }

      private[concurrent] override val batch: Long = batchSize
    }
  }

  /**
   * A strategy that performs no evaluation of its argument.
   *
   * This strategy doesn't work with actors.
   */
  implicit val Id: Strategy = new Strategy {
    def apply[A](a: => A) = () => a
  }

  /**
   * A simple strategy that spawns a new thread for every evaluation.
   */
  implicit val Naive: Strategy = new Strategy {
    def apply[A](a: => A) = {
      val executorService = Executors.newSingleThreadExecutor(Strategy.DefaultDaemonThreadFactory)
      val future = executorService.submit(new Callable[A] {
        def call: A = a
      })
      executorService.shutdown()
      () => future.get
    }

    private[concurrent] override val batch: Long = -1 // maximize batch size to minimize forking of threads
  }

  /**
   * A strategy that evaluates its arguments using the pool of Swing worker threads.
   */
  implicit val SwingWorker: Strategy = new Strategy {

    import javax.swing.SwingWorker

    def apply[A](a: => A) = {
      val worker = new SwingWorker[A, Unit] {
        def doInBackground(): A = a
      }
      worker.execute()
      () => worker.get
    }
  }

  /**
   * A strategy that evaluates its arguments on the Swing Event Dispatching thread.
   */
  implicit val SwingInvokeLater: Strategy = new Strategy {

    import javax.swing.SwingUtilities.invokeLater

    def apply[A](a: => A) = {
      val task = new FutureTask[A](new Callable[A] {
        def call: A = a
      })
      invokeLater(task)
      () => task.get
    }
  }
}
