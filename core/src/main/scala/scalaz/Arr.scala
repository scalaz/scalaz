package scalaz

trait Arr[F[_, _]] {
  def arr[A, B](f: A => B): F[A, B]
}

object Arr extends Arrs

trait Arrs {
  implicit val Function1Arr: Arr[Function1] = new Arr[Function1] {
    def arr[A, B](f: A => B) =
      f
  }

  implicit val PartialFunctionArr: Arr[PartialFunction] = new Arr[PartialFunction] {
    def arr[A, B](f: A => B) = {
      case b => f(b)
    }
  }

  implicit def KleisliArr[F[_]](implicit p: Pointed[F]): Arr[({type λ[α, β] = Kleisli[α, F, β]})#λ] = new Arr[({type λ[α, β] = Kleisli[α, F, β]})#λ] {
    def arr[A, B](f: A => B) =
      Kleisli.kleisli(a => p.point(f(a)))
  }

  implicit def CoKleisliArr[F[_]](implicit p: CoPointed[F]): Arr[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = new Arr[({type λ[α, β] = CoKleisli[α, F, β]})#λ] {
    def arr[A, B](f: A => B) =
      CoKleisli.coKleisli(a => f(p.coPoint(a)))
  }

}