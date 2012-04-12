package scalaz

trait Cozip[F[_]] { self =>
  def cozip[A, B](x: F[Either[A, B]]): Either[F[A], F[B]]
  def cozipT[A, B](x: EitherT[F, A, B]): Either[F[A], F[B]] =
    cozip(x.run)

  def cozip3[A, B, C](x: F[Either[A, Either[B, C]]]): Either[F[A], Either[F[B], F[C]]] =
    cozip(x).right.map(cozip(_))

  def cozip4[A, B, C, D](x: F[Either[A, Either[B, Either[C, D]]]]): Either[F[A], Either[F[B], Either[F[C], F[D]]]] =
    cozip(x).right.map(cozip(_).right map (cozip(_)))

  def cozip5[A, B, C, D, E](x: F[Either[A, Either[B, Either[C, Either[D, E]]]]]): Either[F[A], Either[F[B], Either[F[C], Either[F[D], F[E]]]]] =
    cozip(x).right.map(cozip(_).right map (cozip(_).right map (cozip(_))))

  def cozip6[A, B, C, D, E, G](x: F[Either[A, Either[B, Either[C, Either[D, Either[E, G]]]]]]): Either[F[A], Either[F[B], Either[F[C], Either[F[D], Either[F[E], F[G]]]]]] =
    cozip(x).right.map(cozip(_).right map (cozip(_).right map (cozip(_).right map (cozip(_)))))

  def cozip7[A, B, C, D, E, G, H](x: F[Either[A, Either[B, Either[C, Either[D, Either[E, Either[G, H]]]]]]]): Either[F[A], Either[F[B], Either[F[C], Either[F[D], Either[F[E], Either[F[G], F[H]]]]]]] =
    cozip(x).right.map(cozip(_).right map (cozip(_).right map (cozip(_).right map (cozip(_).right map(cozip(_))))))

}

object Cozip {
  @inline def apply[F[_]](implicit F: Cozip[F]): Cozip[F] = F
}
