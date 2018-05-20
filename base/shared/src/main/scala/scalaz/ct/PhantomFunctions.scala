package scalaz
package ct

trait PhantomFunctions {
  def pmap[F[_], A, B](fa: F[A])(implicit F: Phantom[F]): F[B] = F.pmap(fa)
}
