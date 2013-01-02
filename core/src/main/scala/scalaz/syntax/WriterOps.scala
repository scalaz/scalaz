package scalaz
package syntax

trait WriterOps[A] extends Ops[A] {
  def set[W](w: W): Writer[W, A] = WriterT.writer(w -> self)

  def tell: Writer[A, Unit] = WriterT.tell(self)
}

trait ToWriterOps {
  implicit def ToWriterOps[A](a: A) = new WriterOps[A]{ def self = a }
}
