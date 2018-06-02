package scalaz
package std

import scala.Vector
import scala.Predef.$conforms

import core.EqClass
import ct.MonadClass

trait VectorInstances {
  implicit val vectorMonad: Monad[Vector] = instanceOf(new MonadClass[Vector] {
    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B]             = fa.map(f)
    override def ap[A, B](fa: Vector[A])(f: Vector[A => B]): Vector[B]      = fa.zip(f).map { case (e, f) => f(e) }
    override def pure[A](a: A): Vector[A]                                   = Vector(a)
    override def flatMap[A, B](oa: Vector[A])(f: A => Vector[B]): Vector[B] = oa.flatMap(f)
    override def flatten[A](ma: Vector[Vector[A]]): Vector[A]               = ma.flatten
  })

  implicit def vectorEq[A: Eq]: Eq[Vector[A]] =
    instanceOf(new EqClass[Vector[A]] {
      def equal(first: Vector[A], second: Vector[A]): Boolean = (first.corresponds(second)(Eq[A].equal))
    })

  /* https://github.com/scalaz/scalaz/pull/1633
  implicit def vectorDebug[A: Debug]: Debug[Vector[A]] = instanceOf(new DebugClass[Vector[A]] {
    override def show(as: Vector[A]) = {
      def commaSep(tail: Vector[A], acc: Cord): Cord = tail match {
        case Nil => acc
        case x :: xs => commaSep(xs, (acc :+ ",") ++ Debug[A].debug(x))
      }

      "[" +: (as match {
        case Nil => Cord()
        case x :: xs => commaSep(xs, Debug[A].debug(x))
      }) :+ "]"
    }
  })
 */
}
