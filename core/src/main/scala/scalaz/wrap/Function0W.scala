package scalaz
package wrap

sealed trait Function0W[T] {
  val k: () => T

  import data.Validation, Validation._

  def throws: Validation[Throwable, T] =
    try {
      success(k())
    } catch {
      case e => failure(e)
    }
}

object Function0W extends Function0Ws

trait Function0Ws {
  implicit def Function0To[T](f: () => T): Function0W[T] = new Function0W[T] {
    val k = f
  }

  implicit def Function0From[T](f: Function0W[T]): () => T = f.k
}
