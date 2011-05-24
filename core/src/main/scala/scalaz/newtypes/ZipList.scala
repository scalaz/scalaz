package scalaz
package newtypes

sealed trait ZipList[A] {
  val value: List[A]
}

object ZipList extends ZipLists

trait ZipLists {
  implicit def ZipListUnpack[A]: Newtype[ZipList[A], List[A]] =
    Newtype.newtype(_.value, b => new ZipList[A] {
      val value = b
    })
}
