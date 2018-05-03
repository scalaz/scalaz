package scalaz
package data

import scalaz.typeclass._

trait KleisliInstances {
  import Kleisli.{ runKleisli, wrapKleisli }

  implicit def monad[M[_], A0](implicit M: Monad[M]): Monad[Kleisli[M, A0, ?]] =
    instanceOf(
      new MonadClass[Kleisli[M, A0, ?]] with MonadClass.DeriveMap[Kleisli[M, A0, ?]]
      with BindClass.DeriveFlatten[Kleisli[M, A0, ?]] with BindClass.DeriveAp[Kleisli[M, A0, ?]] {
        def pure[A](a: A): Kleisli[M, A0, A] =
          wrapKleisli(_ => M.pure(a))
        def flatMap[A, B](ma: Kleisli[M, A0, A])(f: A => Kleisli[M, A0, B]): Kleisli[M, A0, B] =
          wrapKleisli(a0 => M.flatMap(runKleisli(ma)(a0))(a => runKleisli(f(a))(a0)))
      }
    )

  implicit def compose[M[_]](implicit M: Monad[M]): Compose[Kleisli[M, ?, ?]] =
    instanceOf(new ComposeClass[Kleisli[M, ?, ?]] {
      def compose[A, B, C](f: Kleisli[M, B, C], g: Kleisli[M, A, B]): Kleisli[M, A, C] =
        wrapKleisli(a => M.flatMap(runKleisli(g)(a))(runKleisli(f)))
    })

  implicit def monoid[M[_], A, B](implicit M: Monoid[M[B]]): Monoid[Kleisli[M, A, B]] =
    instanceOf(new MonoidClass[Kleisli[M, A, B]] {
      override def empty: Kleisli[M, A, B] =
        wrapKleisli(_ => M.empty)

      override def append(a1: Kleisli[M, A, B], a2: => Kleisli[M, A, B]): Kleisli[M, A, B] =
        wrapKleisli(a => M.append(runKleisli(a1)(a), runKleisli(a2)(a)))
    })

  implicit def strong[M[_], A, B](implicit M: Functor[M]): Strong[Kleisli[M, ?, ?]] =
    instanceOf(
      new StrongClass[Kleisli[M, ?, ?]] with StrongClass.DeriveSecond[Kleisli[M, ?, ?]]
      with ProfunctorClass.DeriveLRMap[Kleisli[M, ?, ?]] {
        override def first[A, B, C](pab: Kleisli[M, A, B]): Kleisli[M, (A, C), (B, C)] =
          Kleisli.first(pab)

        override def dimap[A, B, C, D](fab: Kleisli[M, A, B])(ca: C => A)(bd: B => D): Kleisli[M, C, D] =
          wrapKleisli(c => M.map(runKleisli(fab)(ca(c)))(bd))
      }
    )
}
