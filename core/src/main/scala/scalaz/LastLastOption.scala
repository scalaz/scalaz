package scalaz

trait LastLazyOption[A] extends NewType[LazyOption[A]]

trait LastLazyOptions {
  implicit def LastLazyOptionTo[A](a: LazyOption[A]): LastLazyOption[A] = new LastLazyOption[A] {
    val value = a
  }
}
