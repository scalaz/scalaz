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

  ////

  ////
}
