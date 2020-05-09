package scalaz

////
/** The class of monads supporting write operations
 *
 */
////
trait MonadTell[F[_], S] extends Monad[F] { self =>
  ////
  def writer[A](w: S, v: A): F[A]

  def tell(w: S): F[Unit] = writer(w, ())

  ////
  val monadTellSyntax: scalaz.syntax.MonadTellSyntax[F, S] =
    new scalaz.syntax.MonadTellSyntax[F, S] { def F = MonadTell.this }
}

object MonadTell {
  @inline def apply[F[_], S](implicit F: MonadTell[F, S]): MonadTell[F, S] = F

  import Isomorphism._

  def fromIso[F[_], G[_], E](D: F <~> G)(implicit A: MonadTell[G, E]): MonadTell[F, E] =
    new IsomorphismMonadTell[F, G, E] {
      override def G: MonadTell[G, E] = A
      override def iso: F <~> G = D
    }

  ////
  /** The Free instruction set for MonadTell */
  sealed abstract class Ast[S, A]
  final case class Writer[S, A](s: S, a: A) extends Ast[S, A]

  /** Extensible Effect */
  def liftF[F[_], S](
    implicit I: Ast[S, *] :<: F
  ): MonadTell[Free[F, *], S] with BindRec[Free[F, *]] =
    new MonadTell[Free[F, *], S] with BindRec[Free[F, *]] {
      val delegate = Free.freeMonad[F]
      def point[A](a: =>A): Free[F, A] = delegate.point(a)
      def bind[A, B](fa: Free[F, A])(f: A => Free[F, B]) = delegate.bind(fa)(f)
      override def map[A, B](fa: Free[F, A])(f: A => B) = delegate.map(fa)(f)
      override def tailrecM[A, B](f: A => Free[F, A \/ B])(a: A) = delegate.tailrecM(f)(a)

      def writer[A](w: S, v: A): Free[F, A] = Free.liftF(I.inj(Writer[S, A](w, v)))
    }
  ////
}
