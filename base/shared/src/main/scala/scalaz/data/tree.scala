package scalaz
package data

import tc._

sealed abstract class FixFreeModule {
  type FixFree[F[_], A]

  def unwrap[F[_], A](f: FixFree[F, A]): A \/ F[data.FixFree[F, A]]
  def wrap[F[_], A](f: A \/ F[data.FixFree[F, A]]): FixFree[F, A]
  def lift[F[_]: Functor, A](fa: F[A]): FixFree[F, A]
  def pure[F[_], A](a: A): FixFree[F, A]

  def monad[F[_]: Functor]: Monad[FixFree[F, ?]]
}
object FixFreeModule {
  implicit def monadTree[F[_]: Functor]: Monad[FixFree[F, ?]] = FixFree.monad
}

private[data] object FixFreeImpl extends FixFreeModule {
  override type FixFree[F[_], A] = A \/ F[data.FixFree[F, A]]

  override def unwrap[F[_], A](f: FixFree[F, A]): A \/ F[data.FixFree[F, A]] = f
  override def wrap[F[_], A](f: A \/ F[data.FixFree[F, A]]): FixFree[F, A]   = f
  override def lift[F[_]: Functor, A](fa: F[A]): FixFree[F, A] =
    wrap(\/-(Functor[F].map(fa)(a => FixFree.pure[F, A](a))))
  override def pure[F[_], A](a: A): FixFree[F, A] = -\/(a)

  override def monad[F[_]: Functor]: Monad[FixFree[F, ?]] =
    instanceOf(
      new MonadClass[FixFree[F, ?]] {
        override def pure[A](a: A): FixFree[F, A] = FixFreeImpl.pure(a)
        override def flatMap[A, B](fa: FixFree[F, A])(f: A => FixFree[F, B]): FixFree[F, B] =
          unwrap(fa).fold(a => f(a), ffa => wrap(\/-(Functor[F].map(ffa)(fa2 => unevil(flatMap(evil(fa2))(f))))))
      }
    )

  private def evil[F[_], A](dtfa: data.FixFree[F, A]): FixFree[F, A]   = wrap(FixFree.unwrap(dtfa))
  private def unevil[F[_A], A](tfa: FixFree[F, A]): data.FixFree[F, A] = FixFree.wrap(unwrap(tfa))
}
