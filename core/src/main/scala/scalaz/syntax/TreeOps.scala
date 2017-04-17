package scalaz
package syntax

final class TreeOps[A](val self: A) extends AnyVal {
  def node(subForest: Tree[A]*): Tree[A] = Tree.Node(self, subForest.toStream)

  def leaf: Tree[A] = Tree.Leaf(self)
}

trait ToTreeOps {
  implicit def ToTreeOps[A](a: A): TreeOps[A] = new TreeOps(a)
}
