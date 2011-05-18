package scalaz

trait Pointed[F[_]] {
  def point[A](a: => A): F[A]
}

object Pointed extends Pointeds

trait Pointeds {

  import Const._

  implicit def ConstPointed[A: Zero]: Pointed[({type λ[α] = Const[A, α]})#λ] = new Pointed[({type λ[α] = Const[A, α]})#λ] {
    def point[B](a: => B) = const[B](implicitly[Zero[A]].zero)
  }

  implicit val OptionPointed: Pointed[Option] = new Pointed[Option] {
    def point[A](a: => A) = Some(a)
  }

  implicit val ListPointed: Pointed[List] = new Pointed[List] {
    def point[A](a: => A) = List(a)
  }

  implicit val StreamPointed: Pointed[Stream] = new Pointed[Stream] {
    def point[A](a: => A) = Stream(a)
  }

}
