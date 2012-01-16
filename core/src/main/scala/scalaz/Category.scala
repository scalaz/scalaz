package scalaz

////
/**
 *
 */
////
trait Category[=>:[_, _]] extends ArrId[=>:] with Compose[=>:] { self =>

  ////
  // TODO GeneralizedCategory, GeneralizedFunctor, et al, from Scalaz6 ?

  def empty[A]: PlusEmpty[({type λ[α]=(α =>: α)})#λ] = new PlusEmpty[({type λ[α]=(α =>: α)})#λ] with ComposePlus[A] {
    def empty[A] = id
  }
  def monoid[A]: Monoid[A =>: A] = new Monoid[A =>: A] with ComposeSemigroup[A] {
    def zero = id
  }

  trait CategoryLaw extends ComposeLaw {
    def leftIdentity[A, B](ab: (A =>: B))(implicit E: Equal[A =>: B]): Boolean = {
      val ab1 = compose(ab, id[A])
      E.equal(ab, ab1)
    }
    def rightIdentity[A, B](ab: (A =>: B))(implicit E: Equal[A =>: B]): Boolean = {
      val ab1 = compose(id[B], ab)
      E.equal(ab, ab1)
    }
  }

  def categoryLaw = new CategoryLaw {}

  ////
  val categorySyntax = new scalaz.syntax.CategorySyntax[=>:] {}
}

object Category {
  @inline def apply[F[_, _]](implicit F: Category[F]): Category[F] = F
}

