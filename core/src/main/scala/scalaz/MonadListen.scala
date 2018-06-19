package scalaz

trait MonadListen[F[_], W] extends MonadTell[F, W] {
  def listen[A](ma: F[A]): F[(A, W)]

  def pass[A](ma: F[(A, W => W)]): F[A] =
    bind(listen(ma)){ case ((a, f), w) => writer(f(w), a) }

  val monadListenSyntax = new scalaz.syntax.MonadListenSyntax[F, W]{def F = MonadListen.this}
}

object MonadListen {
  def apply[F[_], W](implicit ML: MonadListen[F, W]) = ML

  /** The Free instruction set for MonadListen */
  sealed abstract class Ast[S, A]
  final case class Listen[F[_], S, A](ma: F[A]) extends Ast[S, (A, S)]

  /** Extensible Effect */
  def liftF[F[_], S](
    implicit
      T: MonadTell.Ast[S, ?] :<: F,
      L: Ast[S, ?] :<: F
  ): MonadListen[Free[F, ?], S] with BindRec[Free[F, ?]] =
    new MonadListen[Free[F, ?], S] with BindRec[Free[F, ?]] {
      val delegate = Free.freeMonad[F]
      def point[A](a: =>A): Free[F, A] = delegate.point(a)
      def bind[A, B](fa: Free[F, A])(f: A => Free[F, B]) = delegate.bind(fa)(f)
      override def map[A, B](fa: Free[F, A])(f: A => B) = delegate.map(fa)(f)
      override def tailrecM[A, B](f: A => Free[F, A \/ B])(a: A) = delegate.tailrecM(f)(a)

      def writer[A](w: S, v: A): Free[F, A] = Free.liftF(T.inj(MonadTell.Writer[S, A](w, v)))

      def listen[A](ma: Free[F, A]): Free[F, (A, S)] = Free.liftF(L.inj(Listen[Free[F, ?], S, A](ma)))
    }
}
