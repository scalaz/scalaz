package scalaz
package data

import scalaz.typeclass._

trait KleisliInstances {
  import Kleisli.{ run, wrap }

  implicit def monad[M[_], A0](implicit M: Monad[M]): Monad[Kleisli[M, A0, ?]] =
    instanceOf(
      new MonadClass[Kleisli[M, A0, ?]] with MonadClass.DeriveMap[Kleisli[M, A0, ?]]
      with BindClass.DeriveFlatten[Kleisli[M, A0, ?]] with BindClass.DeriveAp[Kleisli[M, A0, ?]] {
        def pure[A](a: A): Kleisli[M, A0, A] =
          wrap(_ => M.pure(a))
        def flatMap[A, B](ma: Kleisli[M, A0, A])(f: A => Kleisli[M, A0, B]): Kleisli[M, A0, B] =
          wrap(a0 => M.flatMap(run(ma)(a0))(a => run(f(a))(a0)))
      }
    )

  implicit def compose[M[_]](implicit M: Monad[M]): Compose[Kleisli[M, ?, ?]] =
    instanceOf(new ComposeClass[Kleisli[M, ?, ?]] {
      def compose[A, B, C](f: Kleisli[M, B, C], g: Kleisli[M, A, B]): Kleisli[M, A, C] =
        wrap(a => M.flatMap(run(g)(a))(run(f)))
    })

  implicit def monoid[M[_], A, B](implicit M: Monoid[M[B]]): Monoid[Kleisli[M, A, B]] =
    instanceOf(new MonoidClass[Kleisli[M, A, B]] {
      override def empty: Kleisli[M, A, B] =
        wrap(_ => M.empty)

      override def append(a1: Kleisli[M, A, B], a2: => Kleisli[M, A, B]): Kleisli[M, A, B] =
        wrap(a => M.append(run(a1)(a), run(a2)(a)))
    })

  implicit def strong[M[_], A, B](implicit M: Functor[M]): Strong[Kleisli[M, ?, ?]] =
    instanceOf(
      new StrongClass[Kleisli[M, ?, ?]] with StrongClass.DeriveSecond[Kleisli[M, ?, ?]]
      with ProfunctorClass.DeriveLRMap[Kleisli[M, ?, ?]] {
        override def first[A, B, C](pab: Kleisli[M, A, B]): Kleisli[M, (A, C), (B, C)] =
          wrap(t => M.map(run(pab)(t._1))((_, t._2)))

        override def dimap[A, B, C, D](fab: Kleisli[M, A, B])(ca: C => A)(bd: B => D): Kleisli[M, C, D] =
          wrap(c => M.map(run(fab)(ca(c)))(bd))
      }
    )
}
