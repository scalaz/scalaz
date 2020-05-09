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
  /** The Free instruction set for MonadState */
  sealed abstract class Ast[S, A]
  final case class Get[S]()     extends Ast[S, S]
  final case class Put[S](s: S) extends Ast[S, Unit]

  /** Extensible Effect */
  def liftF[F[_], S](
    implicit I: Ast[S, *] :<: F
  ): MonadState[Free[F, *], S] with BindRec[Free[F, *]] =
    new MonadState[Free[F, *], S] with BindRec[Free[F, *]] {
      val delegate = Free.freeMonad[F]
      def point[A](a: =>A): Free[F, A] = delegate.point(a)
      def bind[A, B](fa: Free[F, A])(f: A => Free[F, B]) = delegate.bind(fa)(f)
      override def map[A, B](fa: Free[F, A])(f: A => B) = delegate.map(fa)(f)

      def tailrecM[A, B](a: A)(f: A => Free[F, A \/ B]) = delegate.tailrecM(a)(f)

      def get: Free[F, S] = Free.liftF(I.inj(Get[S]()))
      def put(s: S): Free[F, Unit] = Free.liftF(I.inj(Put[S](s)))
    }
  ////
}

trait IsomorphismMonadState[F[_], G[_], S] extends MonadState[F, S] with IsomorphismMonad[F, G]{
  implicit def G: MonadState[G, S]
  ////

  override def get: F[S] = iso.from(G.get)

  override def put(s: S): F[Unit] = iso.from(G.put(s))

  ////
}
