package scalaz

trait Codiagonal[=>:[_, _]] { self =>
  def codiagonal[A]: Either[A,  A] =>: A
}

object Codiagonal {
  @inline def apply[F[_, _]](implicit F: Codiagonal[F]): Codiagonal[F] = F

  ////
  ////
}
