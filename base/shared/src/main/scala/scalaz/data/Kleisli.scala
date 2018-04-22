package scalaz
package data

sealed trait KleisliModule {
  type Kleisli[F[_], A, B]

  def run[F[_], A, B](k: Kleisli[F, A, B]): A => F[B]

  def wrap[F[_], A, B](k: A => F[B]): Kleisli[F, A, B]

  def hoist[F[_], G[_], A, B](k: Kleisli[F, A, B])(η: F ~> G): Kleisli[G, A, B]

  def compose[F[_], A, B, C](
    k: Kleisli[F, A, B],
    j: Kleisli[F, B, C]
  )(implicit B: Bind[F]): Kleisli[F, A, C]

}

private[data] object KleisliImpl extends KleisliModule with KleisliSyntax with KleisliInstances {
  type Kleisli[F[_], A, B] = A => F[B]

  override def run[F[_], A, B](k: Kleisli[F, A, B]): A => F[B] = k

  override def wrap[F[_], A, B](k: A => F[B]): Kleisli[F, A, B] = k

  override def hoist[F[_], G[_], A, B](k: Kleisli[F, A, B])(η: F ~> G): Kleisli[G, A, B] =
    k andThen η.apply

  override def compose[F[_], A, B, C](
    k: Kleisli[F, A, B],
    j: Kleisli[F, B, C]
  )(implicit B: Bind[F]): Kleisli[F, A, C] =
    a => B.flatMap(k(a))(j)
}
