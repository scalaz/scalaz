package scalaz
package syntax

final class TreeOps[A](val self: A) extends Super {
  def node(subForest: Tree[A]*): Tree[A] = Tree.node(self, subForest.toStream)

  def leaf: Tree[A] = Tree.leaf(self)
}

trait ToTreeOps {
  implicit def ToTreeOps[A](a: A) = new TreeOps(a)
}
