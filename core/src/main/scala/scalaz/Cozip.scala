package scalaz

////
/**
 *
 */
////
trait Cozip[F[_]]  { self =>
  ////
  def cozip[A, B](x: F[Either[A, B]]): Either[F[A], F[B]]

  // derived functions

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

  ////
  val cozipSyntax = new scalaz.syntax.CozipSyntax[F] {}
}

object Cozip {
  @inline def apply[F[_]](implicit F: Cozip[F]): Cozip[F] = F

  ////
  def cofzip[F[_], A, B](x: F[Either[A, B]])(implicit F: Cozip[F]): Either[F[A], F[B]] = F.cozip(x)
  def cofzipT[F[_], A, B](x: EitherT[F, A, B])(implicit F: Cozip[F]): Either[F[A], F[B]] = F.cozipT(x)
  def cofzip3[F[_], A, B, C](x: F[Either[A, Either[B, C]]])(implicit F: Cozip[F]): Either[F[A], Either[F[B], F[C]]] = F.cozip3(x)
  def cofzip4[F[_], A, B, C, D](x: F[Either[A, Either[B, Either[C, D]]]])(implicit F: Cozip[F]): Either[F[A], Either[F[B], Either[F[C], F[D]]]] = F.cozip4(x)
  def cofzip5[F[_], A, B, C, D, E](x: F[Either[A, Either[B, Either[C, Either[D, E]]]]])(implicit F: Cozip[F]): Either[F[A], Either[F[B], Either[F[C], Either[F[D], F[E]]]]] = F.cozip5(x)
  def cofzip6[F[_], A, B, C, D, E, G](x: F[Either[A, Either[B, Either[C, Either[D, Either[E, G]]]]]])(implicit F: Cozip[F]): Either[F[A], Either[F[B], Either[F[C], Either[F[D], Either[F[E], F[G]]]]]] = F.cozip6(x)
  def cofzip7[F[_], A, B, C, D, E, G, H](x: F[Either[A, Either[B, Either[C, Either[D, Either[E, Either[G, H]]]]]]])(implicit F: Cozip[F]): Either[F[A], Either[F[B], Either[F[C], Either[F[D], Either[F[E], Either[F[G], F[H]]]]]]] = F.cozip7(x)
  ////
}

