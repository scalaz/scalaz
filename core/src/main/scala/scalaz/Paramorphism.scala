package scalaz

trait Paramorphism[P[_]] {
  def para[A, B](fa: P[A], b: B, f: (=> A, => P[A], B) => B): B

	// Apply a function to consecutives pairs of an environment and accumulates the result of each application in a new environement
  def pairMap[A](fa: P[A], f: (A, A) => A)(implicit e: Empty[P], p: Pure[P], m: Monoid[P[A]], s: Semigroup[P[A]], first: First[P]) : P[A] = {

    def combine(a: => A, as: => P[A], b: P[A]) : P[A] = first.first(as) match
    {
      case None => b
      case Some(value) => s.append(p.pure(f(a, value)), b)
    }

    para[A, P[A]](fa, e.empty, combine);
  }
}

object Paramorphism {
  implicit val ListParamorphism = new Paramorphism[List] {
    override def para[A, B](as: List[A], b: B, f: ((=> A, => List[A], B) => B)): B = as match {
      case Nil => b
      case a :: as => f(a, as, para(as, b, f))
    }
  }

  implicit val StreamParamorphism = new Paramorphism[Stream] {
    override def para[A, B](as: Stream[A], b: B, f: ((=> A, => Stream[A], B) => B)): B =
      if(as.isEmpty)
        b
      else
        f(as.head, as.tail, para(as.tail, b, f))
  }

  implicit val OptionParamorphism = new Paramorphism[Option] {
    def para[A, B](as: Option[A], b: B, f: ((=> A, => Option[A], B) => B)): B = as match {
      case None => b
      case Some(a) => f(a, None, b)
    }
  }
}
