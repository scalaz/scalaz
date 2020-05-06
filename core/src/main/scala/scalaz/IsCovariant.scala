package scalaz

////
import Liskov._

/**
 * Type constructors that are covariant with respect to subtyping.
 *
 * A type constructor `F[_]` admits an instance `IsCovariant[F]` iff
 * `F[A]` is a subtype of `F[B]` whenever `A` is a subtype of `B`.
 *
 * The Scala language represents this natively using the `[+A]` notation on
 * type constructors. The factory [[IsCovariant.scalaCovariant scalaCovariant]]
 * provides an instance of this typeclass for any such type. However, type
 * constructors which behave covariantly despite not being thus notated can also
 * implement this typeclass. The factory [[IsCovariant.force force]] constructs
 * an instance of `IsCovariant` for any type, based on the programmer's
 * assertion that it is indeed covariant.
 *
 * Note the relationship between this typeclass and [[Functor]]: whereas
 * [[Functor]] instances are covariant functors in the "category" of Scala types
 * and functions, instances of this typeclass are covariant functors in the
 * "category" of Scala types and subtyping relations.
 */
////
trait IsCovariant[F[_]]  { self =>
  ////

  def liftLiskovCo[A, B](implicit ev: A <~< B): F[A] <~< F[B]

  // derived methods
  def substCo[G[+_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] =
    liftLiskovCo(ev).substCo[G](g)

  def substCt[G[-_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] =
    liftLiskovCo(ev).substCt[G](g)

  def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B] =
    liftLiskovCo(ev).apply(fa)

  ////
  val isCovariantSyntax: scalaz.syntax.IsCovariantSyntax[F] =
    new scalaz.syntax.IsCovariantSyntax[F] { def F = IsCovariant.this }
}

object IsCovariant {
  @inline def apply[F[_]](implicit F: IsCovariant[F]): IsCovariant[F] = F



  ////

  implicit def scalaCovariant[F[+_]]: IsCovariant[F] = new IsCovariant[F] {
    def liftLiskovCo[A, B](implicit ev: A <~< B) = co(ev)
  }

  def force[F[_]]: IsCovariant[F] = new IsCovariant[F] {
    def liftLiskovCo[A, B](implicit ev: A <~< B) =
      ev.asInstanceOf[F[A] <~< F[B]]
  }

  ////
}
