package scalaz
package std

import scala.xml.{ NodeSeq, PrettyPrinter }

trait NodeSeqInstances {
  implicit val nodeSeqInstance: Monoid[NodeSeq] with Show[NodeSeq] with Equal[NodeSeq] = new Monoid[NodeSeq] with Show[NodeSeq] with Equal[NodeSeq] {
    def zero = NodeSeq.Empty
    def append(f1: NodeSeq, f2: ⇒ NodeSeq) = f1 ++ f2
    override def shows(as: NodeSeq) = new PrettyPrinter(80, 2) formatNodes as
    def equal(a1: NodeSeq, a2: NodeSeq): Boolean = a1 == a2
  }
}

object nodeseq extends NodeSeqInstances
