package scalaz

////
/**
 *
 */
////
trait Cozip[F[_]]  { self =>
  ////
  def cozip[A, B](x: F[A \/ B]): (F[A] \/ F[B])

  // derived functions
  def cozip3[A, B, C](x: F[A \/ (B \/ C)]): (F[A] \/ (F[B] \/ F[C])) =
    cozip(x).map(cozip(_))

  def cozip4[A, B, C, D](x: F[A \/ (B \/ (C \/ D))]): (F[A] \/ (F[B] \/ (F[C] \/ F[D]))) =
    cozip(x).map(cozip(_) map (cozip(_)))

  def cozip5[A, B, C, D, E](x: F[(A \/ (B \/ (C \/ (D \/ E))))]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ F[E])))) =
    cozip(x).map(cozip(_) map (cozip(_) map (cozip(_))))

  def cozip6[A, B, C, D, E, G](x: F[(A \/ (B \/ (C \/ (D \/ (E \/ G)))))]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ (F[E] \/ F[G]))))) =
    cozip(x).map(cozip(_) map (cozip(_) map (cozip(_) map (cozip(_)))))

  def cozip7[A, B, C, D, E, G, H](x: F[(A \/ (B \/ (C \/ (D \/ (E \/ (G \/ H))))))]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ (F[E] \/ (F[G] \/ F[H])))))) =
    cozip(x).map(cozip(_) map (cozip(_) map (cozip(_) map (cozip(_) map(cozip(_))))))

  ////
  val cozipSyntax = new scalaz.syntax.CozipSyntax[F] { def F = Cozip.this }
}

object Cozip {
  @inline def apply[F[_]](implicit F: Cozip[F]): Cozip[F] = F

  ////
  def cofzip[F[_], A, B](x: F[A \/ B])(implicit F: Cozip[F]): (F[A] \/ F[B]) = F.cozip(x)
  def cofzip3[F[_], A, B, C](x: F[A \/ (B \/ C)])(implicit F: Cozip[F]): (F[A] \/ (F[B] \/ F[C])) = F.cozip3(x)
  def cofzip4[F[_], A, B, C, D](x: F[A \/ (B \/ (C \/ D))])(implicit F: Cozip[F]): (F[A] \/ (F[B] \/ (F[C] \/ F[D]))) = F.cozip4(x)
  def cofzip5[F[_], A, B, C, D, E](x: F[(A \/ (B \/ (C \/ (D \/ E))))])(implicit F: Cozip[F]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ F[E])))) = F.cozip5(x)
  def cofzip6[F[_], A, B, C, D, E, G](x: F[(A \/ (B \/ (C \/ (D \/ (E \/ G)))))])(implicit F: Cozip[F]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ (F[E] \/ F[G]))))) = F.cozip6(x)
  def cofzip7[F[_], A, B, C, D, E, G, H](x: F[(A \/ (B \/ (C \/ (D \/ (E \/ (G \/ H))))))])(implicit F: Cozip[F]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ (F[E] \/ (F[G] \/ F[H])))))) = F.cozip7(x)
  ////
}
