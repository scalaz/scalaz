package scalaz

import scala.{ Any, AnyRef, AnyVal, Nothing, Null }

import Predef._
import prop.<~<
import Scalaz._

class AsResolutionTest {
  implicitly[Int <~< Int]
  implicitly[Int <~< AnyVal]

  implicitly[String <~< String]
  implicitly[String <~< AnyRef]
  implicitly[String <~< Any]

  implicitly[Nothing <~< Int]
  implicitly[Nothing <~< String]
  implicitly[Nothing <~< AnyRef]
  implicitly[Nothing <~< AnyVal]
  implicitly[Nothing <~< Any]

  implicitly[Null <~< String]
  implicitly[Null <~< AnyRef]
  implicitly[Null <~< Any]

  implicitly[(String, Int) <~< (String, Any)]
  implicitly[(String, Int) <~< (Any, Any)]
  implicitly[(String, Int) <~< (AnyRef, AnyVal)]
  implicitly[(String, Int) <~< AnyRef]
  implicitly[(String, Int) <~< Any]
  implicitly[Null <~< (String, Int)]
  implicitly[Nothing <~< (String, Int)]

  trait A { type X }
  def f1(a: A): a.X <~< a.X = implicitly[a.X <~< a.X]

  trait F[L, H >: L] { type A >: L <: (H with B); type B >: L <: H }
  def g1[L, H >: L](a: F[L, H]): a.A <~< a.B = implicitly[a.A <~< a.B]
  def g2[L, H >: L](a: F[L, H]): a.A <~< a.B = implicitly[a.A <~< a.B]

  def h1[A, B >: A]: A <~< B = implicitly[A <~< B]
  def h2[A]: A <~< A         = implicitly[A <~< A]

}
