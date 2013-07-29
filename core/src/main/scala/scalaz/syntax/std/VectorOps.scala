package scalaz
package syntax
package std

final class VectorOps[A](override val self: Vector[A]) extends Super with IndexedSeqOps0[Vector, A] {
  protected def v = scalaz.std.vector
}

trait ToVectorOps {
  implicit def ToVectorOpsFromVector[A](a: Vector[A]): VectorOps[A] = new VectorOps(a)
}
