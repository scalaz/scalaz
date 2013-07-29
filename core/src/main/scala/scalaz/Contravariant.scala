package scalaz

////
/**
 * Contravariant functors.  For example, functions provide a
 * [[scalaz.Functor]] in their result type, but a
 * [[scalaz.Contravariant]] for each argument type.
 *
 * Note that the dual of a [[scalaz.Functor]] is just a [[scalaz.Functor]]
 * itself.
 *
 * Providing an instance of this is a useful alternative to marking a
 * type parameter with `-` in Scala.
 *
 * @see [[scalaz.Contravariant.ContravariantLaw]]
 */
////
trait Contravariant[F[_]] extends InvariantFunctor[F] { self =>
  ////

  /** Transform `A`.
    *
    * @note `contramap(r)(identity)` = `r`
    */
  def contramap[A, B](r: F[A])(f: B => A): F[B]

  // derived functions

  def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B] =
    contramap(fa)(g)

  /** The composition of Contravariant F and G, `[x]F[G[x]]`, is
    * covariant.
    */
  def compose[G[_]](implicit G0: Contravariant[G]): Functor[({type λ[α] = F[G[α]]})#λ] =
    new Functor[({type λ[α] = F[G[α]]})#λ] {
      def map[A, B](fa: F[G[A]])(f: A => B) =
        self.contramap(fa)(gb => G0.contramap(gb)(f))
    }

  /** The composition of Contravariant F and Functor G, `[x]F[G[x]]`,
    * is contravariant.
    */
  def icompose[G[_]](implicit G0: Functor[G]): Contravariant[({type λ[α] = F[G[α]]})#λ] =
    new Contravariant[({type λ[α] = F[G[α]]})#λ] {
      def contramap[A, B](fa: F[G[A]])(f: B => A) =
        self.contramap(fa)(gb => G0.map(gb)(f))
    }

  /** The product of Contravariant `F` and `G`, `[x](F[x], G[x]])`, is
    * contravariant.
    */
  def product[G[_]](implicit G0: Contravariant[G]): Contravariant[({type λ[α] = (F[α], G[α])})#λ] =
    new Contravariant[({type λ[α] = (F[α], G[α])})#λ] {
      def contramap[A, B](fa: (F[A], G[A]))(f: B => A) =
        (self.contramap(fa._1)(f), G0.contramap(fa._2)(f))
    }

  trait ContravariantLaw extends InvariantFunctorLaw {
    /** The identity function, lifted, is a no-op. */
    def identity[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(contramap(fa)(x => x), fa)

    /**
     * A series of contramaps may be freely rewritten as a single
     * contramap on a composed function.
     */
    def composite[A, B, C](fa: F[A], f1: B => A, f2: C => B)(implicit FC: Equal[F[C]]): Boolean = FC.equal(contramap(contramap(fa)(f1))(f2), contramap(fa)(f1 compose f2))
  }
  def contravariantLaw = new ContravariantLaw {}

  ////
  val contravariantSyntax = new scalaz.syntax.ContravariantSyntax[F] { def F = Contravariant.this }
}

object Contravariant {
  @inline def apply[F[_]](implicit F: Contravariant[F]): Contravariant[F] = F

  ////

  ////
}
