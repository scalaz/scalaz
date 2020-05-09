package scalaz

abstract class Codensity[F[_], A] { self =>
  def apply[B](f: A => F[B]): F[B]
  def improve(implicit F: Applicative[F]): F[A] =
    apply(a => F.point(a))
  def flatMap[B](k: A => Codensity[F, B]): Codensity[F, B] = {
    new Codensity[F, B] {
      def apply[C](h: B => F[C]): F[C] =
        self.apply(a => k(a)(h))
    }
  }
  def map[B](k: A => B): Codensity[F, B] =
    flatMap(x => Codensity.pureCodensity(k(x)))

  /** `Codensity[F,_]` is a right Kan extension of `F` along itself. */
  def toRan: Ran[F, F, A] = new Ran[F, F, A] {
    def apply[B](f: A => F[B]) = self(f)
  }
}

object Codensity extends CodensityInstances {
  def rep[F[_], A](f: F[A])(implicit F: Monad[F]): Codensity[F, A] =
    new Codensity[F, A] {
      def apply[B](k: A => F[B]) = F.bind(f)(k)
    }

  def pureCodensity[F[_], A](a: => A): Codensity[F, A] =
    new Codensity[F, A] {
      def apply[B](f: A => F[B]): F[B] = f(a)
    }

  /** Supposing we have the guarantees of consistency between
    * [[scalaz.Applicative]] and [[scalaz.PlusEmpty]] for `F`, the
    * [[scalaz.MonadPlus]] laws should hold.
    */
  implicit def codensityMonadPlus[F[_]](implicit F: ApplicativePlus[F]): MonadPlus[Codensity[F, *]] =
    new CodensityMonad[F] with MonadPlus[Codensity[F, *]] {
      def empty[A] =
        new Codensity[F, A] {
          def apply[B](f: A => F[B]) = F.empty[B]
        }

      def plus[A](a: Codensity[F, A], b: => Codensity[F, A]) =
        new Codensity[F, A] {
          def apply[B](f: A => F[B]) = F.plus(a(f), b(f))
        }
    }

  implicit val codensityTrans: MonadTrans[Codensity] =
    new MonadTrans[Codensity] {
      def liftM[G[_]: Monad, A](a: G[A]) = Codensity.rep(a)
      def apply[G[_]: Monad] = codensityMonad[G]
    }
}

sealed abstract class CodensityInstances {
  implicit def codensityMonad[F[_]]: Monad[Codensity[F, *]] =
    new CodensityMonad[F]
}

private[scalaz] sealed class CodensityMonad[F[_]] extends Monad[Codensity[F, *]] {
  final def point[A](a: => A) = Codensity.pureCodensity(a)

  override final def map[A, B](fa: Codensity[F, A])(f: A => B) =
    fa map f

  final def bind[A, B](fa: Codensity[F, A])(k: A => Codensity[F, B]) =
    fa flatMap k
}
