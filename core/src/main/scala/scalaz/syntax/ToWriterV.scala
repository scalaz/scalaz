package scalaz
package syntax

trait WriterV[A] extends SyntaxV[A] {
  def set[W](w: W): Writer[W, A] = WriterT.writer(w -> self)

  def tell: Writer[A, Unit] = WriterT.tell(self)
}

trait ToWriterV {
  implicit def ToWriterV[A](a: A) = new WriterV[A]{ def self = a }
}
