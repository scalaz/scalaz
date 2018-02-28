package scalaz
package typeclass

trait TraversableInstances {
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

  implicit def tuple2Traversable[C]: Traversable[Tuple2[C, ?]] =
    instanceOf(new TraversableClass.DeriveSequence[Tuple2[C, ?]] with FoldableClass.DeriveFoldMap[Tuple2[C, ?]] {
      def traverse[F[_], A, B](ta: Tuple2[C, A])(f: A => F[B])(implicit F: Applicative[F]): F[Tuple2[C, B]] =
        F.map(f(ta._2))(b => (ta._1, b))

      override def foldLeft[A, B](ta: Tuple2[C, A], z: B)(f: (B, A) => B): B = f(z, ta._2)

      override def foldRight[A, B](ta: Tuple2[C, A], z: => B)(f: (A, => B) => B): B = f(ta._2, z)

      override def toList[A](ta: Tuple2[C, A]): List[A] = List(ta._2)

      override def map[A, B](ta: Tuple2[C, A])(f: A => B): Tuple2[C, B] = (ta._1, f(ta._2))
    })
}
