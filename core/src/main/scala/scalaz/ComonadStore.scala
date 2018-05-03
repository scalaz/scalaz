package scalaz

////
/**
 *
 */
////
trait ComonadStore[F[_], S] extends Comonad[F] { self =>
  ////
  def pos[A](w: F[A]): S
  def peek[A](s: S, w: F[A]): A

  def peeks[A](s: S => S, w: F[A]): A =
    peek(s(pos(w)), w)

  def seek[A](s: S, w: F[A]): F[A] =
    peek(s, cojoin(w))

  def seeks[A](s: S => S, w: F[A]): F[A] =
    peeks(s, cojoin(w))

  def experiment[G[_], A](s: S => G[S], w: F[A])(implicit FG: Functor[G]): G[A] =
    FG.map(s(pos(w)))(peek(_, w))
  ////

}

object ComonadStore {
  @inline def apply[F[_], S](implicit F: ComonadStore[F, S]): ComonadStore[F, S] = F

  ////
  import Isomorphism.<~>

  def fromIso[F[_], G[_], S](D: F <~> G)(implicit E: ComonadStore[G, S]): ComonadStore[F, S] =
    new IsomorphismComonadStore[F, G, S] {
      override implicit def G: ComonadStore[G, S] = E
      override def iso: F <~> G = D
    }
  ////
}
