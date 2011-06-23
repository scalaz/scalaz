package scalaz

trait Category[F[_, _]] {
  val id: Id[F]
  val compose: Compose[F]

  def i[A]: F[A, A] =
    id.id[A]

  def comp[A, B, C](f: F[B, C], g: F[A, B]): F[A, C] =
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

}

