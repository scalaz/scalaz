package scalaz

import scala.{ Any, Nothing }

import Scalaz._

class IsResolutionTest {
  implicitly[Int === Int]
  implicitly[Nothing === Nothing]
  implicitly[Any === Any]

  trait A { type X }
  def f1(a: A): a.X === a.X = implicitly[a.X === a.X]

  def h1[A]: A === A = implicitly[A === A]

  def h2[A, B >: A <: A]: A === B = implicitly[A === B]
}
