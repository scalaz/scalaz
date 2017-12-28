package scalaz
package data

import scala.language.implicitConversions

trait AsInstances {
  /**We can witness equality by using it to convert between types */
  implicit def witness[A, B](lt: A <~< B): A => B = lt(_)
}

