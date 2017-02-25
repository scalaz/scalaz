package scalaz

////
/**
 * [[scalaz.Compose]] with identity.
 *
 * @see [[scalaz.Category.CategoryLaw]]
 */
////
trait Category[=>:[_, _]] extends Compose[=>:] { self =>
  ////
  // TODO GeneralizedCategory, GeneralizedFunctor, et al, from Scalaz6 ?

  /** The left and right identity over `compose`. */
  def id[A]: A =>: A

  /** `monoid`, but universally quantified. */
  def empty: PlusEmpty[λ[α => α =>: α]] =
    new PlusEmpty[λ[α => α =>: α]] with ComposePlus {
      def empty[A] = id
    }

  /** The endomorphism monoid, where `zero`=`id` and
    * `append`=`compose`.
    */
  def monoid[A]: Monoid[A =>: A] =
    new Monoid[A =>: A] with ComposeSemigroup[A] {
      def zero = id
    }

  trait CategoryLaw extends ComposeLaw {
    /** `_ <<< id` is vacuous. */
    def leftIdentity[A, B](ab: (A =>: B))(implicit E: Equal[A =>: B]): Boolean = {
      val ab1 = compose(ab, id[A])
      E.equal(ab, ab1)
    }
    /** `id <<< _` is vacuous. */
    def rightIdentity[A, B](ab: (A =>: B))(implicit E: Equal[A =>: B]): Boolean = {
      val ab1 = compose(id[B], ab)
      E.equal(ab, ab1)
    }
  }

  def categoryLaw = new CategoryLaw {}

  ////
  val categorySyntax = new scalaz.syntax.CategorySyntax[=>:] { def F = Category.this }
}

object Category {
  @inline def apply[F[_, _]](implicit F: Category[F]): Category[F] = F

  ////
  ////
}
