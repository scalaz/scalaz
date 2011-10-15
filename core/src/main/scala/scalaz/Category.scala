package scalaz

// TODO GeneralizedCategory, GeneralizedFunctor, et al, from Scalaz6 ?
trait Category[~>:[_, _]] extends ArrId[~>:] with Compose[~>:]

object Category extends Categorys

trait Categorys {

  // TODO
//  implicit val `<:<_Category` : Category[<:<] =
//    category[<:<]
//
//  implicit val `=:=_Category` : Category[=:=] =
//    category[=:=]
//
//  implicit def CoKleisliCategory[F[_]](implicit cm: CoMonad[F]): Category[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = {
//    implicit val e = cm.extend
//    implicit val p = cm.coPointed
//    Category.category[({type λ[α, β] = CoKleisli[α, F, β]})#λ]
//  }
//
//  implicit def LensCategory: Category[Lens] =
//    Category.category

}
