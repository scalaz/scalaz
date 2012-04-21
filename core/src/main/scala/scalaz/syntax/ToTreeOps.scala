package scalaz
package syntax

trait TreeV[A] extends Ops[A] {
  def node(subForest: Tree[A]*): Tree[A] = Tree.node(self, subForest.toStream)

  def leaf: Tree[A] = Tree.leaf(self)
}

trait ToTreeOps {
  implicit def ToTreeV[A](a: A) = new TreeV[A]{ def self = a }
}
