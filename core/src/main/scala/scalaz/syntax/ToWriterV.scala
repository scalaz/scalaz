package scalaz
package syntax

trait WriterV[A] extends SyntaxV[A] {
  def set[W](w: W): WriterT.Writer[W, A] = WriterT.writer(w -> self)

  def tell: WriterT.Writer[A, Unit] = WriterT.tell(self)
}

trait ToWriterV {
  implicit def ToWriterV[A](a: A) = new WriterV[A]{ def self = a }
}
