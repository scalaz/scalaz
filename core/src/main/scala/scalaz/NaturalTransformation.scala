package scalaz

trait NaturalTransformation[F[_], G[_]] {
  self =>
  def apply[A](fa: F[A]): G[A]

  def compose[E[_]](f: E ~> F): E ~> G = new (E ~> G) {
    def apply[A](ea: E[A]) = self(f(ea))
  }
}

trait NaturalTransformations {
  /**A function type encoded as a natural transformation */
  type ->[A, B] = (({type λ[α]=A})#λ) ~> (({type λ[α]=B})#λ)

  /**A universally quantified identity function */
  def id = new (Id ~> Id) {
    def apply[A](a: A) = a
  }
}


object NaturalTransformation extends NaturalTransformations

trait BinaturalTransformation[F[_, _], G[_, _]] {
  self =>
  def apply[A, B](f: F[A, B]): G[A, B]

  def compose[E[_, _]](f: BinaturalTransformation[E, F]) = new BinaturalTransformation[E, G] {
    def apply[A, B](eab: E[A, B]): G[A, B] = self(f(eab))
  }
}
