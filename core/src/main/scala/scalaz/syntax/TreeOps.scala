package scalaz
package syntax

trait TreeOps[A] extends Ops[A] {
  def node(subForest: Tree[A]*): Tree[A] = Tree.node(self, subForest.toStream)

  def leaf: Tree[A] = Tree.leaf(self)
}

trait ToTreeOps {
  implicit def ToTreeOps[A](a: A) = new TreeOps[A]{ def self = a }
}
