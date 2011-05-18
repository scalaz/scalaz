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

}