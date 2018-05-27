package scalaz
package ct

import scalaz.data.~>

sealed trait KleisliModule {
  type Kleisli[F[_], A, B]

  def runKleisli[F[_], A, B](k: Kleisli[F, A, B]): A => F[B]

  def wrapKleisli[F[_], A, B](k: A => F[B]): Kleisli[F, A, B]

  def hoist[F[_], G[_], A, B](k: Kleisli[F, A, B])(η: F ~> G): Kleisli[G, A, B]

  def first[F[_], A, B, C](k: Kleisli[F, A, B])(implicit F: Functor[F]): Kleisli[F, (A, C), (B, C)]

  def second[F[_], A, B, C](k: Kleisli[F, A, B])(implicit F: Functor[F]): Kleisli[F, (C, A), (C, B)]

  def compose[F[_], A, B, C](j: Kleisli[F, B, C], k: Kleisli[F, A, B])(implicit B: Bind[F]): Kleisli[F, A, C]

}

private[ct] object KleisliImpl extends KleisliModule {
  type Kleisli[F[_], A, B] = A => F[B]

  override def runKleisli[F[_], A, B](k: Kleisli[F, A, B]): A => F[B] = k

  override def wrapKleisli[F[_], A, B](k: A => F[B]): Kleisli[F, A, B] = k

  override def hoist[F[_], G[_], A, B](k: Kleisli[F, A, B])(η: F ~> G): Kleisli[G, A, B] =
    k andThen η.apply

  override def first[F[_], A, B, C](
    k: Kleisli[F, A, B]
  )(implicit F: scalaz.Functor[F]): Kleisli[F, (A, C), (B, C)] =
    wrapKleisli(t => F.map(runKleisli(k)(t._1))((_, t._2)))

  override def second[F[_], A, B, C](
    k: Kleisli[F, A, B]
  )(implicit F: Functor[F]): Kleisli[F, (C, A), (C, B)] =
    wrapKleisli(t => F.map(runKleisli(k)(t._2))((t._1, _)))

  override def compose[F[_], A, B, C](
    j: Kleisli[F, B, C],
    k: Kleisli[F, A, B]
  )(implicit B: Bind[F]): Kleisli[F, A, C] =
    a => B.flatMap(k(a))(j)
}
