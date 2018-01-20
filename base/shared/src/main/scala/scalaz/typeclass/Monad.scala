package scalaz
package typeclass

trait Monad[M[_]] {
  def applicative: Applicative[M] with this.type
  def bind: Bind[M] with this.type
}
