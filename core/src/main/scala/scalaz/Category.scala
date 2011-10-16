package scalaz

trait Category[=>:[_, _]] extends ArrId[=>:] with Compose[=>:] { self =>
  ////
  // TODO GeneralizedCategory, GeneralizedFunctor, et al, from Scalaz6 ?

  ////
  val categorySyntax = new scalaz.syntax.CategorySyntax[=>:] {}
}

object Category {
  def apply[F[_, _]](implicit F: Category[F]): Category[F] = F

  ////
  // TODO
  //  implicit def CokleisliCategory[F[_]](implicit cm: CoMonad[F]): Category[({type λ[α, β] = Cokleisli[α, F, β]})#λ] = {
  //    implicit val e = cm.extend
  //    implicit val p = cm.coPointed
  //    Category.category[({type λ[α, β] = Cokleisli[α, F, β]})#λ]
  //  }
  //
  //  implicit def LensCategory: Category[Lens] =
  //    Category.category
  ////
}

