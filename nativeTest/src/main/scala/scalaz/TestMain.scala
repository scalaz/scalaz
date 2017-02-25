package scalaz

import scalaz.Scalaz._
import scalaz.iteratee.Iteratee._
import scalaz.effect._
import scalaz.Id.Id

object TestMain {
  def main(args: Array[String]): Unit = {
    val list1 = List[Option[Int]](Some(1), Some(2), Some(3), Some(4))
    val res1 = Traverse[List].sequence(list1)
    println(res1)
    assert(res1 == Option(List(1, 2, 3, 4)))

    val i = consume[Int, Id, List] &= enumStream(Stream(1, 2, 3))
    val res2 = i.run
    println(res2)
    assert(res2 == Stream(1, 2, 3))
  }
}
