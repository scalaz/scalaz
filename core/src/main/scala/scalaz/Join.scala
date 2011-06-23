package scalaz

trait Join[F[_]] {
  def join[A]: F[F[A]] => F[A]
}

object Join extends Joins

trait Joins {
  implicit val OptionJoin: Join[Option] = new Join[Option] {
    def join[A] =
      _ flatMap identity
  }

  implicit val ListJoin: Join[List] = new Join[List] {
    def join[A] =
      _ flatMap identity
  }

  implicit val StreamJoin: Join[Stream] = new Join[Stream] {
    def join[A] =
      _ flatMap identity
  }

  implicit val IdentityJoin: Join[Identity] = implicitly[Monad[Identity]].join

  implicit def KleisliJoin[F[_], R](implicit bd: Bind[F]): Join[({type λ[α] = Kleisli[R, F, α]})#λ] = new Join[({type λ[α] = Kleisli[R, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

}
