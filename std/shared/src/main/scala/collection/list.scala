package scalaz
package std

import scala.{ ::, List, Nil }

import scala.Predef.$conforms

import ct.MonadClass
import core.EqAnyRef
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

  implicit def listEq[A: Eq]: Eq[List[A]] = instanceOf(((a, b) => (a corresponds b)(Eq[A].equal)): EqAnyRef[List[A]])

  //instanceOf((first, second) => (first.corresponds(second)(Eq[A].equal)): EqAnyRef[List[A]])

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
