package scalaz

trait Logger[Z[_, _]] {
  import Logger._

  def toWriter[E, A]: Z[E, A] => Writer[LOG[E], A]

  def fromWriter[E, A]: Writer[LOG[E], A] => Z[E, A]
}

import Scalaz._

object Logger {
  type LOG[C] = IndSeq[C]

  implicit val WriterLogger: Logger[({type λ[α, β]=Writer[LOG[α], β]})#λ] = new Logger[({type λ[α, β]= Writer[LOG[α], β]})#λ] {
    def toWriter[E, A] = w => w
    def fromWriter[E, A] = w => w
  }
}
