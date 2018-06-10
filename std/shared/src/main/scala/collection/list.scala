package scalaz
package std

import scala.{ ::, List, Nil }

import scala.Predef.$conforms

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

  implicit val listTraversable: Traversable[List] = instanceOf(
    new TraversableClass.DeriveSequence[List] with FoldableClass.DeriveFoldMap[List] {
      override def traverse[F[_], A, B](ta: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
        ta.foldLeft[F[List[B]]](F.pure(List.empty[B])) { (flb, a) =>
          {
            F.ap(flb)(F.map(f(a))(b => (xs: List[B]) => b :: xs))
          }
        }

      override def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)

      override def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z) { (a, b) =>
        f(a, b)
      }

      override def toList[A](xs: List[A]): List[A] = xs

      override def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
    }
  )
}
