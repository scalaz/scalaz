// Copyright: 2017 Sam Halliday
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

  implicit val default_derives: Derives[Default] =
    new ApplicativePlus[Default] {
      override def point[A](a: =>A): Default[A] = instance(a)
      override def ap[A, B](fa: =>Default[A])(
        f: =>Default[A => B]
      ): Default[B] = instance(f.default(fa.default))

      override def empty[A]: Default[A] = ???
      override def plus[A](a: Default[A], b: => Default[A]): Default[A] = a

//      override def coapply1[Z, A1](a1: =>Default[A1])(f: A1 => Z): Default[Z] =
//        instance(f(a1.default))
//      override def coapply2[Z, A1, A2](a1: =>Default[A1], a2: =>Default[A2])(
//        f: A1 \/ A2 => Z
//      ): Default[Z] = instance(f(-\/(a1.default)))

    }

}
