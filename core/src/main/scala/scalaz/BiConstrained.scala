package scalaz

/**A constrained transformation natural in both sides of a bifunctor **/
trait BiConstrained[F[_, _], G[_, _], C[_], E[_]] {
  def apply[A: C, B: E](f: F[A, B]): G[A, B]
}
