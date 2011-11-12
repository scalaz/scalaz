package scalaz

////
/**
 *
 */
////
trait Category[=>:[_, _]] extends ArrId[=>:] with Compose[=>:] { self =>
  ////
  // TODO GeneralizedCategory, GeneralizedFunctor, et al, from Scalaz6 ?

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

