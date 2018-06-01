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

  import Isomorphism._

  def fromIso[F[_], G[_], E](D: F <~> G)(implicit A: MonadState[G, E]): MonadState[F, E] =
    new IsomorphismMonadState[F, G, E] {
      override def G: MonadState[G, E] = A
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismMonadState[F[_], G[_], S] extends MonadState[F, S] with IsomorphismMonad[F, G]{
  implicit def G: MonadState[G, S]
  ////

  override def get: F[S] = iso.from(G.get)

  override def put(s: S): F[Unit] = iso.from(G.put(s))

  ////
}
