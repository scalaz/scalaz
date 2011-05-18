package scalaz
package newtypes

sealed trait ZipList[A] {
  val value: List[A]
}

object ZipList extends ZipLists

trait ZipLists {
  implicit def ZipListUnpack[A]: Unpack[ZipList[A], List[A]] = new Unpack[ZipList[A], List[A]] {
    val unpack = (_: ZipList[A]).value
  }

  implicit def ZipListPack[A]: Pack[ZipList[A], List[A]] = new Pack[ZipList[A], List[A]] {
    val pack = (b: List[A]) => new ZipList[A] {
      val value = b
    }
  }

  implicit def ZipListNewtype[A]: Newtype[ZipList[A], List[A]] =
    Newtype.newtype
}
