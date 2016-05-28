package scalaz
package clazz

trait FoldableInstances {
  implicit val list: Foldable[List] = new Foldable[List] {
    override def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)
    override def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z) { (a, b) => f(a, b) }
    override def toList[A](xs: List[A]): List[A] = xs
  }
}

