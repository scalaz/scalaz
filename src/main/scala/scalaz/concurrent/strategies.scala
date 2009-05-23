package scalaz.concurrent

import java.util.concurrent.Callable

object strategies {

  /**
   * A simple strategy that spawns a new thread for every evaluation.
   */
  object Naive {
    import scala.concurrent.ops._
    implicit def strategy[A] = new Strategy[A] {
      def apply(a: () => A) = future {a()}
    }
  }

  /**
   * A strategy that performs no evaluation of its argument.
   */
  object Id {
    implicit def strategy[A] = new Strategy[A] {
      def apply(a: () => A) = a
    }
  }

  /**
   * A strategy that evaluates its arguments using an implicit java.util.concurrent.ExecutorService.
   */
  object Executor {
    import java.util.concurrent.ExecutorService
    implicit def strategy[A](implicit s: ExecutorService) = new Strategy[A] {
      def apply(a: () => A) = {
        val fut = s.submit(new Callable[A] {
          def call = a()
        })
        () => fut.get
      }
    }
  }

  /**
   * A strategy that evaluates its argument in the current thread.
   */
  object Sequential {
    implicit def strategy[A] = new Strategy[A] {
      def apply(a: () => A) = {
        val v = a()
        () => v
      }
    }
  }

}