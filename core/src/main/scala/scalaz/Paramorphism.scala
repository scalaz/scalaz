package scalaz

trait Paramorphism[P[_]] {
  def para[A, B](fa: P[A], b: B, f: (=> A, => P[A], B) => B): B
}

object Paramorphism {
  implicit def ListParamorphism = new Paramorphism[List] {
    override def para[A, B](as: List[A], b: B, f: ((=> A, => List[A], B) => B)): B = as match {
      case Nil => b
      case a :: as => f(a, as, para(as, b, f))
    }
  }

  implicit def StreamParamorphism = new Paramorphism[Stream] {
    override def para[A, B](as: Stream[A], b: B, f: ((=> A, => Stream[A], B) => B)): B =
      if(as.isEmpty)
        b
      else
        f(as.head, as.tail, para(as.tail, b, f))
  }

  implicit def OptionParamorphism = new Paramorphism[Option] {
    def para[A, B](as: Option[A], b: B, f: ((=> A, => Option[A], B) => B)): B = as match {
      case None => b
      case Some(a) => f(a, None, b)
    }
  }
}
