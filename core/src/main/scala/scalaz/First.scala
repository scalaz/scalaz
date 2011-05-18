package scalaz

trait First[F[_, _]] {
  def first[A, B, C](f: F[A, B]): F[(A, C), (B, C)]
}

object First extends Firsts

trait Firsts {
  implicit val Function1First: First[Function1] = new First[Function1] {
    def first[A, B, C](f: A => B) =
      ac => (f(ac._1), ac._2)
  }

  implicit val PartialFunctionFirst: First[PartialFunction] = new First[PartialFunction] {
    def first[A, B, C](f: PartialFunction[A, B]) = {
      case (a, c) if f isDefinedAt a => (f(a), c)
    }
  }
}