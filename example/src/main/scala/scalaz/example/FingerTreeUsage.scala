package scalaz
package example


object FingerTreeUsage extends App{
  import fingerTree._
  import std.anyVal._
  import std.stream._
  import syntax.foldable._

  def streamToTree[A](stream: Stream[A]): FingerTree[Int, A] = stream.foldLeft(empty(SizeReducer[A])) {
    case (t, x) => (t :+ x)
  }

  implicit def SizeReducer[A]: Reducer[A, Int] = UnitReducer(x => 1)

  val emptyTree = empty[Int, Int](SizeReducer[Int])

  assert(emptyTree.isEmpty)

  assert((emptyTree :+ 2).toList == List(2))

  assert((emptyTree :+ 2 :+ 3).toList == List(2, 3))

  println(streamToTree(Stream.from(1).take(20)))
}