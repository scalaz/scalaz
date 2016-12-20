package scalaz

////
/** The class of monads supporting the operations of
 * [[scalaz.State]].
 *
 */
////
trait MonadState[F[_], S] extends Monad[F] { self =>
  ////

  def get: F[S]
  def put(s: S): F[Unit]

  def state[A](f: S => (S, A)): F[A] = bind(init)(s => f(s) match { case (s, a) => bind(put(s))(_ => point(a)) })
  def constantState[A](a: A, s: => S): F[A] = bind(put(s))(_ => point(a))
  def init: F[S] = get
  def gets[A](f: S => A): F[A] = bind(init)(s => point(f(s)))
  def modify(f: S => S): F[Unit] = bind(init)(s => put(f(s)))

  ////

}

object MonadState {
  @inline def apply[F[_], S](implicit F: MonadState[F, S]): MonadState[F, S] = F

  ////

  def promotedMonadState[G[_], F[_], S](
    implicit
    mpo: MonadPartialOrder[G, F],
    ms: MonadState[F, S]
  ): MonadState[G, S] = new MonadState[G, S] {

    override def get: G[S] = mpo.promote(ms.get)
    override def put(s: S): G[Unit] = mpo.promote(ms put s)

    override def point[A](a: => A): G[A] = mpo.MG point a
    override def bind[A, B](ga: G[A])(f: A => G[B]): G[B] = mpo.MG.bind(ga)(f)

  }

  ////
}
