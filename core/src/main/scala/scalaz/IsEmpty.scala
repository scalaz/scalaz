package scalaz

////
/**
 * Typeclass that permits testing whether some type with an empty representation
 * is in fact empty.
 */
////
trait IsEmpty[F[_]] extends PlusEmpty[F] { self =>
  ////
  def isEmpty[A](fa: F[A]): Boolean

  // derived functions

  trait IsEmptyLaw extends EmptyLaw {
    def emptyIsEmpty[A]: Boolean = isEmpty(empty[A])

    def emptyPlusIdentity[A](f1: F[A], f2: F[A]): Boolean =
      (isEmpty(f1) && isEmpty(f2)) == isEmpty(plus(f1, f2))
  }

  def isEmptyLaw = new IsEmptyLaw {}

  ////
  val isEmptySyntax = new scalaz.syntax.IsEmptySyntax[F] { def F = IsEmpty.this }
}

object IsEmpty {
  @inline def apply[F[_]](implicit F: IsEmpty[F]): IsEmpty[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: IsEmpty[G]): IsEmpty[F] =
    new IsomorphismIsEmpty[F, G] {
      override def G: IsEmpty[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismIsEmpty[F[_], G[_]] extends IsEmpty[F] with IsomorphismPlusEmpty[F, G]{
  implicit def G: IsEmpty[G]
  ////

  def isEmpty[A](fa: F[A]): Boolean =
    G.isEmpty(iso.to(fa))
  ////
}
