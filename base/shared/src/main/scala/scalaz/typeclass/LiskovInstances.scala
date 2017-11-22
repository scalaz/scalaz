package scalaz
package typeclass

import scala.language.implicitConversions

import Prelude.<~<

trait LiskovInstances {
  /**We can witness equality by using it to convert between types */
  implicit def witness[A, B](lt: A <~< B): A => B = lt(_)
}

