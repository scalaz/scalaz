package scalaz
package syntax

final class WriterOps[A](private val self: A) extends AnyVal {
  def set[W](w: W): Writer[W, A] = WriterT.writer(w -> self)

  def tell: Writer[A, Unit] = WriterT.tell(self)
}

trait ToWriterOps {
  implicit def ToWriterOps[A](a: A): WriterOps[A] = new WriterOps(a)
}
