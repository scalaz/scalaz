package scalaz

trait Cokleisli[W[_], A, B] {
  def apply(a: W[A]): B

  def |>=(a: W[A]) = apply(a)

  import MA._
  
  def <<=(a: W[A])(implicit w: Comonad[W]) = {
    ma[W](a) =>> apply
  }
}

object Cokleisli {
  sealed trait CokleisliApply[W[_]] {
    def apply[A, B](f: W[A] => B): Cokleisli[W, A, B]
  }

  def cokleisli[W[_]] = new CokleisliApply[W] {
    def apply[A, B](f: W[A] => B) = new Cokleisli[W, A, B] {
      def apply(a: W[A]) = f(a)
    }
  }
}