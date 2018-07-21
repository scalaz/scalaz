// Copyright: 2017 - 2018 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz

import java.lang.String

import scala.{ inline, Boolean, Int }

// a simple covariant typeclass
trait Default[A] {
  def default: A
}
object Default {
  @inline def apply[A](implicit i: Default[A]): Default[A] = i
  @inline def instance[A](a: =>A): Default[A] = new Default[A] {
    override def default: A = a
  }

  implicit val int: Default[Int]         = instance(0)
  implicit val string: Default[String]   = instance("")
  implicit val boolean: Default[Boolean] = instance(false)

  implicit val default_alt: Alt[Default] =
    new Alt[Default] {
      override def point[A](a: =>A): Default[A] = instance(a)
      override def ap[A, B](fa: =>Default[A])(
        f: =>Default[A => B]
      ): Default[B] = instance(f.default(fa.default))

      override def alt[A](a: =>Default[A], b: =>Default[A]): Default[A] = a
    }

}
