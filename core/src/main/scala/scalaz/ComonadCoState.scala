package scalaz

trait ComonadStore[F[_, _], S] extends Comonad[({type f[x]=F[S,x]})#f] {
  def pos[A](w: F[S, A]): S
  def peek[A](s: S, w: F[S, A]): A

  def peeks[A](s: S => S, w: F[S, A]): A =
    peek(s(pos(w)), w)

  def seek[A](s: S, w: F[S, A]): F[S, A] =
    peek(s, cojoin(w))

  def seeks[A](s: S => S, w: F[S, A]): F[S, A] =
    peeks(s, cojoin(w))

  def experiment[G[_], A](s: S => G[S], w: F[S, A])(implicit FG: Functor[G]): G[A] =
    FG.map(s(pos(w)))(peek(_, w))
}

object ComonadStore {
  def apply[F[_, _],S](implicit F: ComonadStore[F, S]): ComonadStore[F, S] = F
}
