package scalaz

trait ArrId[F[A, B]] {
  def id[A]: F[A, A]
}