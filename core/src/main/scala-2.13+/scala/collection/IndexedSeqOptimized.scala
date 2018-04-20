package scala.collection

import scala.collection.mutable.Builder

trait IndexedSeqOptimized[+A, +Repr] extends Any {
  protected[this] def newBuilder: Builder[A, Repr]
}
