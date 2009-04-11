package scalaz

trait Paramorphism[P[_]] {
  def para[A, B](fa: P[A], b: B, f: (=> A, => P[A], B) => B): B
}

object Paramorphism {
  implicit val OptionParamorphism = new Paramorphism[Option] {
    def para[A, B](as: Option[A], b: B, f: ((=> A, => Option[A], B) => B)): B = as match {
      case None => b
      case Some(a) => f(a, None, b)
    }
  }
}
