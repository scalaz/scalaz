package scalaz
package example

object FingerTreeUsage extends App{
  import FingerTree._
  import std.anyVal._

  def lazyListToTree[A](list: LazyList[A]): FingerTree[Int, A] = list.foldLeft(empty[Int, A]) {
    case (t, x) => (t :+ x)
  }

  val intLazyList = LazyList.from(1)

  implicit def SizeReducer[A]: Reducer[A, Int] = UnitReducer(x => 1)

  val emptyTree = empty[Int, Int]

  assert(emptyTree.isEmpty)

  //prepending
  assert((2 +: 3 +: 4 +: emptyTree).toList == List(2, 3, 4))

  //appending
  assert((emptyTree :+ 2 :+ 3 :+ 4).toList == List(2, 3, 4))

  //folding
  assert(lazyListToTree(intLazyList.take(20)).foldRight(0)(_ + _) == (1 to 20).sum)

  //replace the first element of the tree
  assert((5 |-: lazyListToTree(intLazyList.take(3))).toList == List(5, 2, 3))

  //replace the last element of the tree
  assert((lazyListToTree(intLazyList.take(3)) :-| 5).toList == List(1, 2, 5))

  //appending two trees
  assert((lazyListToTree(intLazyList.take(5)) <++> lazyListToTree(LazyList.from(6).take(5))).toLazyList == intLazyList.take(10))

  import std.option._

  //traversing the tree
  val traversedTree = lazyListToTree(intLazyList.take(10)).traverseTree[Option, Int, Int](i => Some(i * 2))
  assert(traversedTree.map(_.toLazyList).getOrElse(LazyList.empty) == intLazyList.map(_ * 2).take(10))

  println(lazyListToTree(intLazyList.take(10)).traverseTree[Option, Int, Int](i => Some(i + 1)))
}
