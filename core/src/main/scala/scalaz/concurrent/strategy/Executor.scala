package scalaz
package concurrent
package strategy

import java.util.concurrent.{ExecutorService, Callable}

/**
 * A strategy that evaluates its arguments using an implicit java.util.concurrent.ExecutorService.
 */
object Executor {
  implicit def strategy[A](implicit s: ExecutorService): Strategy[A] = new Strategy[A] {
    def apply(a: () => A) = {
      val fut = s.submit(new Callable[A] {
        def call = a()
      })
      () => fut.get
    }
  }
}
