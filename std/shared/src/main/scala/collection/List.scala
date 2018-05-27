package scalaz
package std

import scala.List

import scala.Predef.$conforms

import scalaz.core.EqClass
import ct.MonadClass

trait ListInstances {
  implicit val listMonad: Monad[List] = instanceOf(new MonadClass[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B]           = fa.map(f)
    override def ap[A, B](fa: List[A])(f: List[A => B]): List[B]      = fa.zip(f).map { case (e, f) => f(e) }
    override def pure[A](a: A): List[A]                               = List(a)
    override def flatMap[A, B](oa: List[A])(f: A => List[B]): List[B] = oa.flatMap(f)
    override def flatten[A](ma: List[List[A]]): List[A]               = ma.flatten
  })

  implicit def listEq[A: Eq]: Eq[List[A]] =
    instanceOf(new EqClass[List[A]] {
      def equal(first: List[A], second: List[A]): Boolean = (first.corresponds(second)(Eq[A].equal))
    })

  /* https://github.com/scalaz/scalaz/pull/1633
  implicit def listDebug[A: Debug]: Debug[List[A]] = instanceOf(new DebugClass[List[A]] {
    override def show(as: List[A]) = {
      def commaSep(tail: List[A], acc: Cord): Cord = tail match {
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
