package scalaz

trait FirstLazyOption[A] extends NewType[LazyOption[A]]

trait FirstLazyOptions {
  implicit def LazyFirstOptionTo[A](a: LazyOption[A]): FirstLazyOption[A] = new FirstLazyOption[A] {
    val value = a
  }
}
