package scalaz

////
import Liskov._

/**
  * Type constructors that are contravariant with respect to subtyping.
  *
  * A type constructor `F[_]` admits an instance `IsContravariant[F]` iff
  * `F[B]` is a subtype of `F[A]` whenever `A` is a subtype of `B`.
  *
  * The Scala language represents this natively using the `[-A]` notation on
  * type constructors. The factory [[IsContravariant.scalaContravariant scalaContravariant]]
  * provides an instance of this typeclass for any such type. However, type
  * constructors which behave contravariantly despite not being thus notated can also
  * implement this typeclass. The factory [[IsContravariant.force force]] constructs
  * an instance of `IsContravariant` for any type, based on the programmer's
  * assertion that it is indeed contravariant.
  *
  * Note the relationship between this typeclass and [[Contravariant]]: whereas
  * [[Contravariant]] instances are contravariant functors in the "category" of Scala types
  * and functions, instances of this typeclass are contravariant functors in the
  * "category" of Scala types and subtyping relations.
  */
////
trait IsContravariant[F[_]]  { self =>
  ////

  def liftLiskovCt[A, B](implicit ev: A <~< B): F[B] <~< F[A]

  // derived methods
  def substCo[G[+_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] =
    liftLiskovCt(using ev).substCo[G](g)

  def substCt[G[-_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] =
    liftLiskovCt(using ev).substCt[G](g)

  def narrow[A, B](fa: F[B])(implicit ev: A <~< B): F[A] =
    liftLiskovCt(using ev).apply(fa)

  ////
  val isContravariantSyntax: scalaz.syntax.IsContravariantSyntax[F] =
    new scalaz.syntax.IsContravariantSyntax[F] { def F = IsContravariant.this }
}

object IsContravariant {
  @inline def apply[F[_]](implicit F: IsContravariant[F]): IsContravariant[F] = F



  ////

  /* implicit */
  def scalaContravariant[F[-_]]: IsContravariant[F] = new IsContravariant[F] {
    def liftLiskovCt[A, B](implicit ev: A <~< B) = contra(ev)
  }

  def force[F[_]]: IsContravariant[F] = new IsContravariant[F] {
    def liftLiskovCt[A, B](implicit ev: A <~< B) =
      ev.asInstanceOf[F[B] <~< F[A]]
  }

  ////
}
