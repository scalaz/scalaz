package scalaz
package ct

trait ComonadClass[F[_]] extends CobindClass[F] {
  def copoint[A](fa: F[A]): A
}
