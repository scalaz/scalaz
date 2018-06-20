package scalaz
package std

import scala.{ ::, List, Nil }

import scala.Predef.$conforms

import scalaz.kernel.instanceOf
import core.EqClass
import ct.MonadClass
import data.Cord
import debug.DebugClass

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

  implicit def listDebug[A: Debug]: Debug[List[A]] =
    DebugClass.instance(as => {
      def commaSep(tail: List[A], acc: Cord): Cord = tail match {
        case Nil     => acc
        case x :: xs => commaSep(xs, Cord.concat(acc, Cord.cons(",", Debug[A].debug(x))))
      }

      Cord.wrap("List(", as match {
        case Nil     => Cord.empty
        case x :: xs => commaSep(xs, Debug[A].debug(x))
      }, ")")
    })
}
