package scalaz
package typeclass

import scala.language.implicitConversions

import Liskov.<~<

trait LiskovInstances {
  /**We can witness equality by using it to convert between types */
  implicit def witness[A, B](lt: A <~< B): A => B = {
    type f[-X] = X => B
    lt.subst[f](identity)
  }
}

