package scalaz

////
/**
 * A context in which exceptions can be caught and thrown.
 *
 * This class places no other class constraints on `F`, but it should be the
 * case that exceptions raised via `fail` are caught by the nearest surrounding
 * `attempt` and returned as a `Left`. In addition to catching explicitly
 * raised exceptions via `fail`, we expect that `attempt` catch ambient
 * exceptions that might occur when 'evaluating' an `F`.
 *
 * We can state the requirement that `attempt` catch all ambient exceptions
 * by stipulating that for all total functions of the form
 * `g: forall A . F[Throwable \/ A] => B`, `g compose attempt` is also
 * total.
 */
////
@deprecated("No laws, and Throwable is not referentially transparent. Prefer MonadError", "7.3.0")
trait Catchable[F[_]]  { self =>
  ////

  def attempt[A](f: F[A]): F[Throwable \/ A]
  def fail[A](err: Throwable): F[A]
  // derived functions

  ////
  val catchableSyntax = new scalaz.syntax.CatchableSyntax[F] { def F = Catchable.this }
}

object Catchable {
  @inline def apply[F[_]](implicit F: Catchable[F]): Catchable[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Catchable[G]): Catchable[F] =
    new IsomorphismCatchable[F, G] {
      override def G: Catchable[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismCatchable[F[_], G[_]] extends Catchable[F] {
  implicit def G: Catchable[G]
  ////
  import Isomorphism._

  def iso: F <~> G

  override def attempt[A](f: F[A]): F[Throwable \/ A] =
    iso.from(G.attempt(iso.to(f)))

  override def fail[A](err: Throwable): F[A] =
    iso.from(G.fail(err))
  ////
}
