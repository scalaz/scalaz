package scalaz
package newtypes

sealed trait ZipList[A] {
  val value: List[A]
}

object ZipList extends ZipLists

trait ZipLists {
  implicit def ZipListUnpack[A]: ^*^[ZipList[A], List[A]] =
    ^*^.^*^(_.value, b => new ZipList[A] {
      val value = b
    })
}
