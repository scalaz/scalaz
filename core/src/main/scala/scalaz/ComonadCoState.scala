package scalaz

trait ComonadCostate[F[_, _], S] extends Comonad[({type f[x]=F[S,x]})#f] {
  def pos[A](w: F[S, A]): S
  def peek[A](s: S, w: F[S, A]): A
  def peeks[A](s: S => S, w: F[S, A]): A
  def seek[A](s: S, w: F[S, A]): F[S, A]
  def seeks[A](s: S => S, w: F[S, A]): F[S, A]
}

object ComonadCostate {
  def apply[F[_,_],S](implicit F: ComonadCostate[F, S]) = F
}
