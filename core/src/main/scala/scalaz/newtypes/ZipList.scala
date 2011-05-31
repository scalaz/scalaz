package scalaz
package newtypes

sealed trait ZipList[A] {
  val value: List[A]
}

object ZipList extends ZipLists

trait ZipLists {
  implicit def ZipList_^*^[A]: ^*^[ZipList[A], List[A]] =
    ^*^.^*^(_.value, b => new ZipList[A] {
      val value = b
    })

  implicit def ZipList_^**^ : ^**^[ZipList, List] =
    new ^**^[ZipList, List] {
      def unpack[A] = _.value

      def pack[A] = b => new ZipList[A] {
        val value = b
      }
    }

}
