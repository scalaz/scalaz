package scalaz

trait Iterables {
  implicit def JavaIterableTo[A](i: java.lang.Iterable[A]): Iterable[A] = new Iterable[A] {
    val k = i.iterator
    override def iterator = new Iterator[A] {
      def hasNext = k.hasNext
      def next = k.next
    }
  }

  implicit def JavaIterableFrom[A](k: Iterable[A]): java.lang.Iterable[A] = new java.lang.Iterable[A] {
    val i = k.iterator
    def iterator = new java.util.Iterator[A] {
      def hasNext = i.hasNext
      def next = i.next
      def remove = error("not supported")
    }
  }
}
