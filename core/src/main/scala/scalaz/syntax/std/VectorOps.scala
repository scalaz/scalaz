package scalaz
package syntax
package std

trait ToVectorOps {
  implicit def ToVectorOpsFromVector[A](a: Vector[A]): IndexedSeqOps[Vector, A] = new IndexedSeqOps[Vector, A](a, scalaz.std.vector)
}
