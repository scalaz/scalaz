package scalaz

////
/**
 * [[scalaz.Applicative]] combined with [[scalaz.PlusEmpty]].
 */
////
trait ApplicativePlus[F[_]] extends Applicative[F] with PlusEmpty[F] { self =>
  ////

  /**The composition of ApplicativePlus `F` and Applicative `G`, `[x]F[G[x]]`, is a ApplicativePlus */
  override def compose[G[_]](implicit G0: Applicative[G]): ApplicativePlus[λ[α => F[G[α]]]] =
    new CompositionApplicativePlus[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /**The product of ApplicativePlus `F` and `G`, `[x](F[x], G[x]])`, is a ApplicativePlus */
  def product[G[_]](implicit G0: ApplicativePlus[G]): ApplicativePlus[λ[α => (F[α], G[α])]] =
    new ProductApplicativePlus[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  private[this] class Mutual[A](a: F[A]) {
    lazy val y: Free.Trampoline[F[List[A]]] = z map (plus(_, point(Nil)))
    lazy val z: Free.Trampoline[F[List[A]]] = y map (apply2(a, _)(_ :: _))
  }

  /** `empty` or a non-empty list of results acquired by repeating `a`. */
  def some[A](a: F[A]): F[List[A]] = new Mutual(a).z.run

  /** A list of results acquired by repeating `a`.  Never `empty`;
    * initial failure is an empty list instead.
    */
  def many[A](a: F[A]): F[List[A]] = new Mutual(a).y.run

  ////
  val applicativePlusSyntax = new scalaz.syntax.ApplicativePlusSyntax[F] { def F = ApplicativePlus.this }
}

object ApplicativePlus {
  @inline def apply[F[_]](implicit F: ApplicativePlus[F]): ApplicativePlus[F] = F

  ////

  ////
}
