package scalaz

////
/**
 *
 */
////
trait Compose[=>:[_, _]]  { self =>
  ////

  /** Associative `=>:` binary operator. */
  def compose[A, B, C](f: B =>: C, g: A =>: B): (A =>: C)

  protected[this] trait ComposePlus extends Plus[λ[α => α =>: α]] {
    def plus[A](f1: (A =>: A), f2: => (A =>: A)) = self.compose(f1, f2)
  }
  protected[this] trait ComposeSemigroup[A] extends Semigroup[A =>: A] {
    def append(f1: (A =>: A), f2: => (A =>: A)) = self.compose(f1, f2)
  }

  /** `semigroup`, but universally quantified. */
  def plus: Plus[λ[α => α =>: α]] = new ComposePlus {}
  /** The endomorphism semigroup, where `append`=`compose`. */
  def semigroup[A]: Semigroup[A =>: A] = new ComposeSemigroup[A] {}

  trait ComposeLaw {
    /** `compose` is associative. */
    def associative[A, B, C, D](ab: (A =>: B), bc: (B =>: C), cd: (C =>: D))
                               (implicit E: Equal[A =>: D]): Boolean = {
      val ad1 = compose(cd, compose(bc, ab))
      val ad2 = compose(compose(cd, bc), ab)
      E.equal(ad1, ad2)
    }
  }

  def composeLaw = new ComposeLaw {}

  ////
  val composeSyntax: scalaz.syntax.ComposeSyntax[=>:] =
    new scalaz.syntax.ComposeSyntax[=>:] { def F = Compose.this }
}

object Compose {
  @inline def apply[F[_, _]](implicit F: Compose[F]): Compose[F] = F

  import Isomorphism._

  def fromIso[F[_, _], G[_, _]](D: F <~~> G)(implicit E: Compose[G]): Compose[F] =
    new IsomorphismCompose[F, G] {
      override def G: Compose[G] = E
      override def iso: F <~~> G = D
    }

  ////
  ////
}
