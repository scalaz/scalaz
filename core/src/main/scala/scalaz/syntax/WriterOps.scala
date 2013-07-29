package scalaz
package syntax

final class WriterOps[A](val self: A) extends Super {
  def set[W](w: W): Writer[W, A] = WriterT.writer(w -> self)

  def tell: Writer[A, Unit] = WriterT.tell(self)
}

trait ToWriterOps {
  implicit def ToWriterOps[A](a: A) = new WriterOps(a)
}
