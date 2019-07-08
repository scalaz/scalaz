package scalaz
package syntax

final class TreeOps[A](private val self: A) extends AnyVal {
  def node(subForest: Tree[A]*): Tree[A] = Tree.Node(self, EphemeralStream.fromStream(subForest.toStream))

  def leaf: Tree[A] = Tree.Leaf(self)
}

trait ToTreeOps {
  implicit def ToTreeOps[A](a: A): TreeOps[A] = new TreeOps(a)
}
