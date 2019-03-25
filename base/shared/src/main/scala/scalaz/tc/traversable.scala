package scalaz
package tc

import Predef._

import scala.{ List, Tuple2 }

import scala.language.experimental.macros

trait TraversableClass[T[_]] extends FunctorClass[T] with FoldableClass[T] {

  def traverse[F[_]: Applicative, A, B](ta: T[A])(f: A => F[B]): F[T[B]] = sequence(map(ta)(f))

  def sequence[F[_]: Applicative, A](ta: T[F[A]]): F[T[A]] = traverse(ta)(identity)

  def compose[G[_]](implicit G: TraversableClass[G]): Traversable[λ[α => T[G[α]]]] =
    instanceOf(new CompositionTraversableClass[T, G]()(this, G))
}

object TraversableClass {
  implicit val listTraversable: Traversable[List] =
    instanceOf(instances.list.control)

  implicit def tuple2Traversable[C]: Traversable[Tuple2[C, ?]] =
    instanceOf(new TraversableClass[Tuple2[C, ?]] {
      override def traverse[F[_], A, B](ta: Tuple2[C, A])(f: A => F[B])(implicit F: Applicative[F]): F[Tuple2[C, B]] =
        F.map(f(ta._2))(b => (ta._1, b))

      override def foldLeft[A, B](ta: Tuple2[C, A], z: B)(f: (B, A) => B): B = f(z, ta._2)

      override def foldRight[A, B](ta: Tuple2[C, A], z: => B)(f: (A, => B) => B): B = f(ta._2, z)

      override def toList[A](ta: Tuple2[C, A]): List[A] = List(ta._2)

      override def map[A, B](ta: Tuple2[C, A])(f: A => B): Tuple2[C, B] = (ta._1, f(ta._2))
    })
}

trait TraversableFunctions {
  def sequence[T[_], F[_], A](tfa: T[F[A]])(implicit F: Applicative[F], T: Traversable[T]): F[T[A]] =
    T.sequence(tfa)
}

trait TraversableSyntax {
  implicit final class ToTraversableOps[T[_], A](self: T[A]) {
    def traverse[F[_], B](f: A => F[B])(implicit g: Applicative[F], ev: Traversable[T]): F[T[B]] =
      macro ops.Ops.i_1_1i
  }
}

private class CompositionTraversableClass[F[_], G[_]](implicit F: TraversableClass[F], G: TraversableClass[G])
    extends CompositionFunctorClass[F, G]
    with TraversableClass[λ[α => F[G[α]]]] {

  def foldLeft[A, B](fga: F[G[A]], z: B)(f: (B, A) => B): B =
    F.foldLeft(fga, z) { (b, ga) =>
      G.foldLeft(ga, b)(f)
    }

  def foldRight[A, B](fga: F[G[A]], z: => B)(f: (A, => B) => B): B =
    F.foldRight(fga, z) { (ga, b) =>
      G.foldRight(ga, b)(f)
    }

  override def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
    F.traverse(fga)(G.traverse(_)(f))
}
