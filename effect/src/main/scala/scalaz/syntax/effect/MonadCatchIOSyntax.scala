package scalaz
package syntax
package effect

import scalaz.effect.MonadCatchIO

/** Wraps a value `self` and provides methods related to `MonadCatchIO` */
trait MonadCatchIOOps[F[_],A] extends Ops[F[A]] {
  implicit def F: MonadCatchIO[F]
  ////
  def except(handler: Throwable ⇒ F[A]): F[A] = F.except(self)(handler)
  def onException[B](action: F[B]): F[A] = MonadCatchIO.onException(self, action)
  def bracket[B, C](after: A ⇒ F[B])(during: A ⇒ F[C]): F[C] = 
    MonadCatchIO.bracket(self)(after)(during)
  def ensuring[B](sequel: F[B]): F[A] = MonadCatchIO.ensuring(self, sequel)
  def bracket_[B, C](after: F[B])(during: F[C]): F[C] =
    MonadCatchIO.bracket_(self)(after)(during)
  def bracketOnError[B, C](after: A ⇒ F[B])(during: A ⇒ F[C]): F[C] =
    MonadCatchIO.bracketOnError(self)(after)(during)
  ////
}

trait ToMonadCatchIOOps0 {
  implicit def ToMonadCatchIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[MonadCatchIO, FA]) =
    new MonadCatchIOOps[F0.M,F0.A] { def self = F0(v); implicit def F: MonadCatchIO[F0.M] = F0.TC }

}

trait ToMonadCatchIOOps extends ToMonadCatchIOOps0 {
  implicit def ToMonadCatchIOOps[F[_],A](v: F[A])(implicit F0: MonadCatchIO[F]) =
    new MonadCatchIOOps[F,A] { def self = v; implicit def F: MonadCatchIO[F] = F0 }

  ////

  ////
}

trait MonadCatchIOSyntax[F[_]] {
  implicit def ToMonadCatchIOOps[A](v: F[A])(implicit F0: MonadCatchIO[F]): MonadCatchIOOps[F, A] = new MonadCatchIOOps[F,A] { def self = v; implicit def F: MonadCatchIO[F] = F0 }

  ////

  ////
}
