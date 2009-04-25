package scalaz

trait Length[-L[_]] {
  def len[A](a: L[A]): Int
}

object Length {
  implicit val IdentityLength = new Length[Identity] {
    def len[A](a: Identity[A]) = 1
  }

  implicit def NonEmptyListLength[A] = new Length[NonEmptyList] {
    def len[A](a: NonEmptyList[A]) = a.list.length 
  }

  implicit val ZipStreamLength = new Length[ZipStream] {
    def len[A](a: ZipStream[A]) = a.value.length 
  }

  implicit val Tuple1Length = new Length[Tuple1] {
    def len[A](a: Tuple1[A]) = 1
  }

  implicit val Function0Length = new Length[Function0] {
    def len[A](a: Function0[A]) = 1
  }

  implicit val OptionLength = new Length[Option] {
    def len[A](a: Option[A]) = a map (_ => 1) getOrElse 0
  }

  implicit val ArrayLength = new Length[Array] {
    def len[A](a: Array[A]) = a.length
  }

  implicit val IterableLength: Length[Iterable] = new Length[Iterable] {
    def len[A](a: Iterable[A]) = {
      var n = 0
      val i = a.elements
      while(i.hasNext) {
        n = n + 1
        i.next
      }

      n
    }
  }

  implicit val JavaIterableLength: Length[java.lang.Iterable] = new Length[java.lang.Iterable] {
    def len[A](a: java.lang.Iterable[A]) = {
      var n = 0
      val i = a.iterator
      while(i.hasNext) {
        n = n + 1
        i.next
      }

      n
    }
  }
}