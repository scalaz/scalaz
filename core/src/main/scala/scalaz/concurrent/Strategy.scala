package scalaz
package concurrent

import Scalaz._

/**
 * Evaluate an expression in some specific manner. A typical strategy will schedule asynchronous
 * evaluation and return a function that, when called, will block until the result is ready.
 */
trait Strategy {
  def apply[A](a: => A): () => A
}
