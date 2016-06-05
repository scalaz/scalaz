package scalaz
package example

import collection.immutable.Stream


object FingerTreeUsage extends App{
  import FingerTree._
  import std.anyVal._

  def streamToTree[A](stream: Stream[A]): FingerTree[Int, A] = stream.foldLeft(empty(SizeReducer[A])) {
    case (t, x) => (t :+ x)
  }

  val intStream = Stream.from(1)

  implicit def SizeReducer[A]: Reducer[A, Int] = UnitReducer(x => 1)

  val emptyTree = empty[Int, Int](SizeReducer[Int])

  assert(emptyTree.isEmpty)

  //prepending
  assert((2 +: 3 +: 4 +: emptyTree).toList == List(2, 3, 4))

  //appending
  assert((emptyTree :+ 2 :+ 3 :+ 4).toList == List(2, 3, 4))

  //folding
  assert(streamToTree(intStream.take(20)).foldRight(0)(_ + _) == (1 to 20).sum)

  //replace the first element of the tree
  assert((5 |-: streamToTree(intStream.take(3))).toList == List(5, 2, 3))

  //replace the last element of the tree
  assert((streamToTree(intStream.take(3)) :-| 5).toList == List(1, 2, 5))

  //appending two trees
  assert((streamToTree(intStream.take(5)) <++> streamToTree(Stream.from(6).take(5))).toStream == intStream.take(10))

  import std.option._

  //traversing the tree
  val traversedTree = streamToTree(intStream.take(10)).traverseTree[Option, Int, Int](i => Some(i * 2))
  assert(traversedTree.map(_.toStream).getOrElse(Stream.empty) == intStream.map(_ * 2).take(10))

  println(streamToTree(intStream.take(10)).traverseTree[Option, Int, Int](i => Some(i + 1)))
}
