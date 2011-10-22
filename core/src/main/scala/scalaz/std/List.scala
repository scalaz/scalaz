package scalaz
package std

trait Lists {
  implicit val list = new MonadPlus[List] with Traverse[List] with Empty[List] with Each[List] with Index[List] with Length[List] {
    def each[A](fa: List[A])(f: (A) => Unit): Unit = fa foreach f
    def index[A](fa: List[A], i: Int): Option[A] = {
      var n = 0
      var k: Option[A] = None
      val it = fa.iterator
      while (it.hasNext && k.isEmpty) {
        val z = it.next
        if (n == i) k = Some(z)
        n = n + 1
      }

      k
    }
    def length[A](fa: List[A]): Int = fa.length
    def pure[A](a: => A): List[A] = scala.List(a)
    def bind[A, B](fa: List[A])(f: A => List[B]) = fa flatMap f
    def empty[A] = scala.List()
    def plus[A](a: List[A], b: => List[A]) = a ++ b
    override def map[A, B](l: List[A])(f: A => B) = l map f

    def traverseImpl[F[_], A, B](l: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] = {
      val fbs: F[List[B]] = F.pure(scala.List[B]())

      l.reverse.foldLeft(fbs)((fbl, a) => F.map2(f(a), fbl)(_ :: _))
    }

    def foldR[A, B](fa: List[A], z: B)(f: (A) => (=> B) => B): B = fa.foldRight(z)((a, b) => f(a)(b))
  }

  implicit def listMonoid[A] = new Monoid[List[A]] {
    def append(f1: List[A], f2: => List[A]): List[A] = f1 ::: f2
    def zero: List[A] = Nil
  }
}

object List extends Lists
