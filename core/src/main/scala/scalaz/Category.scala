package scalaz

trait Category[⇝[_, _]] {
  val id: Id[⇝]
  val compose: Compose[⇝]

  def i[A]: (A ⇝ A) =
    id.id[A]

  def comp[A, B, C](f: B ⇝ C, g: A ⇝ B): (A ⇝ C) =
    compose.compose(f, g)
}

object Category extends Categorys

trait Categorys {
  def category[F[_, _]](implicit ii: Id[F], c: Compose[F]): Category[F] = new Category[F] {
    val id = ii
    val compose = c
  }

  implicit val Function1Category: Category[Function1] =
    category[Function1]

  implicit val PartialFunctionCategory: Category[PartialFunction] =
    category[PartialFunction]

  implicit val `<:<_Category` : Category[<:<] =
    category[<:<]

  implicit val `=:=_Category` : Category[=:=] =
    category[=:=]

  implicit def KleisliCategory[F[_]](implicit md: Monad[F]): Category[({type λ[α, β] = Kleisli[α, F, β]})#λ] = {
    implicit val p = md.pointed
    implicit val b = md.bind
    Category.category[({type λ[α, β] = Kleisli[α, F, β]})#λ]
  }

  implicit def CoKleisliCategory[F[_]](implicit cm: CoMonad[F]): Category[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = {
    implicit val e = cm.extend
    implicit val p = cm.coPointed
    Category.category[({type λ[α, β] = CoKleisli[α, F, β]})#λ]
  }

  implicit def LensCategory: Category[Lens] =
    Category.category

}

