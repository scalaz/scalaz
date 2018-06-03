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
    cozip(x).map(cozip)

  def cozip4[A, B, C, D](x: F[A \/ (B \/ (C \/ D))]): (F[A] \/ (F[B] \/ (F[C] \/ F[D]))) =
    cozip(x).map(cozip(_) map cozip)

  def cozip5[A, B, C, D, E](x: F[(A \/ (B \/ (C \/ (D \/ E))))]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ F[E])))) =
    cozip(x).map(cozip(_) map (cozip(_) map cozip))

  def cozip6[A, B, C, D, E, G](x: F[(A \/ (B \/ (C \/ (D \/ (E \/ G)))))]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ (F[E] \/ F[G]))))) =
    cozip(x).map(cozip(_) map (cozip(_) map (cozip(_) map cozip)))

  def cozip7[A, B, C, D, E, G, H](x: F[(A \/ (B \/ (C \/ (D \/ (E \/ (G \/ H))))))]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ (F[E] \/ (F[G] \/ F[H])))))) =
    cozip(x).map(cozip(_) map (cozip(_) map (cozip(_) map (cozip(_) map cozip))))

  ////
  val cozipSyntax = new scalaz.syntax.CozipSyntax[F] { def F = Cozip.this }
}

object Cozip {
  @inline def apply[F[_]](implicit F: Cozip[F]): Cozip[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Cozip[G]): Cozip[F] =
    new IsomorphismCozip[F, G] {
      override def G: Cozip[G] = E
      override def iso: F <~> G = D
    }

  ////
  def cofzip[F[_], A, B](x: F[A \/ B])(implicit F: Cozip[F]): (F[A] \/ F[B]) = F.cozip(x)
  def cofzip3[F[_], A, B, C](x: F[A \/ (B \/ C)])(implicit F: Cozip[F]): (F[A] \/ (F[B] \/ F[C])) = F.cozip3(x)
  def cofzip4[F[_], A, B, C, D](x: F[A \/ (B \/ (C \/ D))])(implicit F: Cozip[F]): (F[A] \/ (F[B] \/ (F[C] \/ F[D]))) = F.cozip4(x)
  def cofzip5[F[_], A, B, C, D, E](x: F[(A \/ (B \/ (C \/ (D \/ E))))])(implicit F: Cozip[F]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ F[E])))) = F.cozip5(x)
  def cofzip6[F[_], A, B, C, D, E, G](x: F[(A \/ (B \/ (C \/ (D \/ (E \/ G)))))])(implicit F: Cozip[F]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ (F[E] \/ F[G]))))) = F.cozip6(x)
  def cofzip7[F[_], A, B, C, D, E, G, H](x: F[(A \/ (B \/ (C \/ (D \/ (E \/ (G \/ H))))))])(implicit F: Cozip[F]): (F[A] \/ (F[B] \/ (F[C] \/ (F[D] \/ (F[E] \/ (F[G] \/ F[H])))))) = F.cozip7(x)
  ////
}

trait IsomorphismCozip[F[_], G[_]] extends Cozip[F] {
  implicit def G: Cozip[G]
  ////
  import Isomorphism._

  def iso: F <~> G

  def cozip[A, B](x: F[A \/ B]): (F[A] \/ F[B]) =
    G.cozip(iso.to(x)).bimap(iso.from.apply _, iso.from.apply _)
  ////
}
