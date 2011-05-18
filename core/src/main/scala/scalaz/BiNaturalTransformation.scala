package scalaz

/**A transformation natural in both sides of a bifunctor **/
trait ~~>[F[_, _], G[_, _]] {
  def apply[A, B](f: F[A, B]): G[A, B]
}
