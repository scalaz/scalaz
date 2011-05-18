package scalaz

/**A constrained natural transformation **/
trait Constrained[F[_], G[_], E[_]] {
  def apply[A: E](f: F[A]): G[A]
}
