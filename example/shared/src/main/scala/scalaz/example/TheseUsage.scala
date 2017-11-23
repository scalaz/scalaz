package scalaz
package example

import data._, typeclass._

object TheseUsage extends App with Instances {
  import These._
  import Functor._
  import Foldable._

  /* creation */
  val one = This[Int, String](1)
  val foo = That[Int, String]("foo")
  val both = Both(1, "foo")

  /* functor instance maps on right */
  assert(one.map(s => s + s) == one)
  assert(foo.map(s => s + s) == That("foofoo"))
  assert(both.map(s => s + s) == Both(1, "foofoo"))

  /* bifunctor */
  assert(one.bimap(_ + 1)(_ + 1)  == This(2))
  assert(foo.bimap(_ + 1)(_ + 1)  == That("foo1"))
  assert(both.bimap(_ + 1)(_ + 1) == Both(2, "foo1"))

  /* foldable instance folds on the right */
  assert(one.toList  == Nil)
  assert(foo.toList  == "foo" :: Nil)
  assert(both.toList == "foo" :: Nil)

  /* monoid instance adds both sides */
  assert(one.append(one)   == This(2))
  assert(one.append(foo)   == Both(1, "foo"))
  assert(one.append(both)  == Both(2, "foo"))
  assert(foo.append(one)   == Both(1, "foo"))
  assert(foo.append(foo)   == That("foofoo"))
  assert(foo.append(both)  == Both(1, "foofoo"))
  assert(both.append(one)  == Both(2, "foo"))
  assert(both.append(foo)  == Both(1, "foofoo"))
  assert(both.append(both) == Both(2, "foofoo"))

}

trait Instances {
  implicit val monoidInt: Monoid[Int] = new MonoidClass[Int] {
    def append(a1: Int, a2: => Int) = a1 + a2
    def empty = 0
  }
}