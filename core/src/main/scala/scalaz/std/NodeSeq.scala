package scalaz
package std

import scala.xml.{ Node, NodeSeq, PrettyPrinter }

trait NodeSeqInstances {
  implicit val nodeSeqInstance: Monoid[NodeSeq] with Show[NodeSeq] with Equal[NodeSeq] = new Monoid[NodeSeq] with Show[NodeSeq] with Equal[NodeSeq] {
    def zero = NodeSeq.Empty
    def append(f1: NodeSeq, f2: ⇒ NodeSeq) = f1 ++ f2
    def show(as: NodeSeq) = new PrettyPrinter(80, 2) formatNodes as toList
    def equal(a1: NodeSeq, a2: NodeSeq): Boolean = a1 == a2
  }
}

object nodeseq extends NodeSeqInstances
