package scalaz
package syntax

final class StrictTreeOps[A](private val self: A) extends AnyVal {
  def strictNode(subForest: StrictTree[A]*): StrictTree[A] = StrictTree(self, subForest.toList)

  def strictLeaf: StrictTree[A] = StrictTree.Leaf(self)
}

trait ToStrictTreeOps {
  implicit def ToStrictTreeOps[A](a: A): StrictTreeOps[A] = new StrictTreeOps(a)
}
