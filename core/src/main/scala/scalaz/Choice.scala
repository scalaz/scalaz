package scalaz

trait Choice[=>:[_, _]] { self =>
  def choice[A, B, C](f: => A =>: C, g: => B =>: C): Either[A,  B] =>: C
}

object Choice {
  @inline def apply[F[_, _]](implicit F: Choice[F]): Choice[F] = F

  ////
  ////
}
