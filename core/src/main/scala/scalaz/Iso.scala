package scalaz


trait =~~=[F[_], G[_]] {
  def ~~=>[A](a: F[A]): G[A]
  def <=~~[A](a: G[A]): F[A]
}

object =~~= extends ==~~==

trait ==~~== {
  implicit def Iso[F[_]]: (F =~~= F) = new (F =~~= F) {
    def ~~=>[A](a: F[A]) = a
    def <=~~[A](a: F[A]) = a
  }

  implicit def ~~=>[F[_], A](a: F[A])(implicit i: F =~~= Identity): A =
    (i ~~=> a).value

  implicit def <=~~[F[_], A](a: A)(implicit i: F =~~= Identity): F[A] =
    i <=~~ Identity.id(a)

}
