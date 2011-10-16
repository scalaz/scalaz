package scalaz

trait Category[~>:[_, _]] extends ArrId[~>:] with Compose[~>:] {
  ////
  // TODO GeneralizedCategory, GeneralizedFunctor, et al, from Scalaz6 ?

  ////
}

object Category {

  ////
  // TODO
  //  implicit val `<:<_Category` : Category[<:<] =
  //    category[<:<]
  //
  //  implicit val `=:=_Category` : Category[=:=] =
  //    category[=:=]k
  //
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
