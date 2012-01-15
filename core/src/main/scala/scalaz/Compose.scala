package scalaz

////
/**
 *
 */
////
trait Compose[=>:[_, _]]  { self =>
  ////
  def compose[A, B, C](f: B =>: C, g: A =>: B): (A =>: C)

  private[scalaz] trait ComposePlus[A] extends Plus[({type λ[α]=(α =>: α)})#λ] {
    def plus[A](f1: (A =>: A), f2: => (A =>: A)) = compose(f1, f2)
  }
  private[scalaz] trait ComposeSemigroup[A] extends Semigroup[A =>: A] {
    def append(f1: (A =>: A), f2: => (A =>: A)) = compose(f1, f2)
  }

  def plus[A]: Plus[({type λ[α]=(α =>: α)})#λ] = new ComposePlus[A] {}
  def semigroup[A]: Semigroup[A =>: A] = new ComposeSemigroup[A] {}

  ////
  val composeSyntax = new scalaz.syntax.ComposeSyntax[=>:] {}
}

object Compose {
  @inline def apply[F[_, _]](implicit F: Compose[F]): Compose[F] = F
}

