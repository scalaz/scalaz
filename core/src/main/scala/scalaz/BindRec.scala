package scalaz

import Isomorphism._

////
/**
 * [[scalaz.Bind]] capable of using constant stack space when doing recursive
 * binds.
 *
 * Implementations of `tailrecM` should not make recursive calls without the
 * `@tailrec` annotation.
 *
  * Based on Phil Freeman's work on stack safety in PureScript, described in
  * [[http://functorial.com/stack-safety-for-free/index.pdf Stack Safety for
  * Free]].
 */
////
trait BindRec[F[_]] extends Bind[F] { self =>
  ////

  def tailrecM[A, B](a: A)(f: A => F[A \/ B]): F[B]

  override def forever[A, B](fa: F[A]): F[B] =
    tailrecM[Unit, B](())(u => map(fa)(_ => -\/(u)))

  /**The product of BindRec `F` and `G`, `[x](F[x], G[x]])`, is a BindRec */
  def product[G[_]](implicit G0: BindRec[G]): BindRec[λ[α => (F[α], G[α])]] =
    new ProductBindRec[F, G] {
      def F = self
      def G = G0
    }

  trait BindRecLaw extends BindLaw {
    def tailrecBindConsistency[A](a: A, f: A => F[A])(implicit FA: Equal[F[A]]): Boolean = {
      val bounce = tailrecM[(Boolean, A), A]((false, a)) {
        case (bounced, a0) =>
          if (!bounced)
            map(f(a0))(a1 => -\/((true, a1)))
          else
            map(f(a0))(\/.right)
      }

      FA.equal(bind(f(a))(f), bounce)
    }
  }
  def bindRecLaw = new BindRecLaw {}

  ////
  val bindRecSyntax = new scalaz.syntax.BindRecSyntax[F] { def F = BindRec.this }
}

trait MonadRecSafely[M[_], A] {
  def apply[S[_]: BindRec: Applicative](lift: M <~> S): S[A]
}

object BindRec {
  @inline def apply[F[_]](implicit F: BindRec[F]): BindRec[F] = F

  def safely[M[_], A](f: MonadRecSafely[M, A])(implicit M: BindRec[M], MA: Applicative[M]): M[A] = {
    def iso[F[_]](implicit FBR: BindRec[F], FA: Applicative[F]): F <~> Free[F, ?] =
      new (F <~> Free[F, ?]) {
        val from: Free[F, ?] ~> F =
          new (Free[F, ?] ~> F) {
            def apply[A](fa: Free[F, A]): F[A] =
              fa.runRecM(a => a)(FA, FA, FBR)
          }
        val to: F ~> Free[F, ?] =
          new (F ~> Free[F, ?]) {
            def apply[A](fa: F[A]): Free[F, A] =
              Free.liftF(fa)
          }
      }

    iso[M].from(f(iso[M]))
  }

  def traverse_[F[_], M[_], A](fa: F[A])(f: A => M[Unit])
               (implicit F: Foldable[F], M: BindRec[M], MA: Applicative[M]): M[Unit] =
    safely(
      new MonadRecSafely[M, Unit] {
        def apply[S[_]: BindRec: Applicative](lift: M <~> S): S[Unit] =
          F.traverse_(fa)(a => lift.to(f(a)))
      }
    )

  ////

  ////
}
