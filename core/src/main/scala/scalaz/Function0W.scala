package scalaz

sealed trait Function0W[T] {
  val k: () => T

  import Scalaz._

  def throws: Validation[Throwable, T] = try {success(k())} catch {case e => failure(e)}
}

trait Function0s {
  implicit def Function0To[T](f: () => T): Function0W[T] = new Function0W[T] {
    val k = f
  }

  implicit def Function0From[T](f: Function0W[T]): () => T = f.k
}
