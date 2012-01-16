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

  trait CategoryLaw {
    def leftIdentity[A, B](ab: (A =>: B))(implicit E: Equal[A =>: B]): Boolean = {
      val ab1 = compose(ab, id[A])
      E.equal(ab, ab1)
    }
    def rightIdentity[A, B](ab: (A =>: B))(implicit E: Equal[A =>: B]): Boolean = {
      val ab1 = compose(id[B], ab)
      E.equal(ab, ab1)
    }
    def associative[A, B, C, D](ab: (A =>: B), bc: (B =>: C), cd: (C =>: D))
                               (implicit E: Equal[A =>: D]): Boolean = {
      val ad1 = compose(cd, compose(bc, ab))
      val ad2 = compose(compose(cd, bc), ab)
      E.equal(ad1, ad2)
    }
  }

  def categoryLaw = new CategoryLaw {}

  ////
  val categorySyntax = new scalaz.syntax.CategorySyntax[=>:] {}
}

object Category {
  @inline def apply[F[_, _]](implicit F: Category[F]): Category[F] = F

  ////
  // TODO
  //  implicit def LensCategory: Category[Lens] =
  //    Category.category
  ////
}

