package scalaz

////
/**
 *
 */
////
trait Unzip[F[_]]  { self =>
  ////
  def unzip[A, B](a: F[(A, B)]): (F[A], F[B])

  // derived functions

  def firsts[A, B](a: F[(A, B)]): F[A] = unzip(a)._1
  def seconds[A, B](a: F[(A, B)]): F[B] = unzip(a)._2

  /**The composition of Unzips `F` and `G`, `[x]F[G[x]]`, is an Unzip */
  def compose[G[_]](implicit T0: Functor[F], G0: Unzip[G]): Unzip[λ[α => F[G[α]]]] =
    new CompositionUnzip[F, G] {
      override def F = self
      override def T = T0
      override def G = G0
    }

  /**The product of Unzips `F` and `G`, `[x](F[x], G[x]])`, is an Unzip */
  def product[G[_]](implicit G0: Unzip[G]): Unzip[λ[α => (F[α], G[α])]] =
    new ProductUnzip[F, G] {
      override def F = self
      override def G = G0
    }

  def unzip3[A, B, C](x: F[(A, (B, C))]): (F[A], F[B], F[C]) = {
    val (a, bc) = unzip(x)
    val (b, c) = unzip(bc)
    (a, b, c)
  }

  def unzip4[A, B, C, D](x: F[(A, (B, (C, D)))]): (F[A], F[B], F[C], F[D]) = {
    val (a, b, cd) = unzip3(x)
    val (c, d) = unzip(cd)
    (a, b, c, d)
  }

  def unzip5[A, B, C, D, E](x: F[(A, (B, (C, (D, E))))]): (F[A], F[B], F[C], F[D], F[E]) = {
    val (a, b, c, de) = unzip4(x)
    val (d, e) = unzip(de)
    (a, b, c, d, e)
  }

  def unzip6[A, B, C, D, E, G](x: F[(A, (B, (C, (D, (E, G)))))]): (F[A], F[B], F[C], F[D], F[E], F[G]) = {
    val (a, b, c, d, eg) = unzip5(x)
    val (e, g) = unzip(eg)
    (a, b, c, d, e, g)
  }

  def unzip7[A, B, C, D, E, G, H](x: F[(A, (B, (C, (D, (E, (G, (H)))))))]): (F[A], F[B], F[C], F[D], F[E], F[G], F[H]) = {
    val (a, b, c, d, e, gh) = unzip6(x)
    val (g, h) = unzip(gh)
    (a, b, c, d, e, g, h)
  }

  ////
  val unzipSyntax: scalaz.syntax.UnzipSyntax[F] =
    new scalaz.syntax.UnzipSyntax[F] { def F = Unzip.this }
}

object Unzip {
  @inline def apply[F[_]](implicit F: Unzip[F]): Unzip[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Unzip[G]): Unzip[F] =
    new IsomorphismUnzip[F, G] {
      override def G: Unzip[G] = E
      override def iso: F <~> G = D
    }

  ////
  implicit def idInstance: Unzip[Id.Id] = Id.id
  ////
}
