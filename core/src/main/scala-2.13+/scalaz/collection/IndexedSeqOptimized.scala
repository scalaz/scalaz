package scalaz.collection

import scala.collection.mutable.Builder

private[scalaz] trait IndexedSeqOptimized[+A, +Repr] extends Any {
  protected[this] def newBuilder: Builder[A, Repr]
}
