package scalaz

object TracedTTest extends SpecLite {

  def compilationTestTracedTU: Unit = {
    import scalaz.syntax.either._
    import scalaz.std.function._

    val a: Int \/ (Byte => String) = 1.left[Byte => String]
    TracedT.tracedTU(a)
  }

  object instances {
    def functor[F[_]: Functor, A] = Functor[TracedT[F, A, ?]]
    def apply[F[_]: Apply, A] = Apply[TracedT[F, A, ?]]
    def applicative[F[_]: Applicative, A] = Applicative[TracedT[F, A, ?]]
    def distributive[F[_]: Distributive, A] = Distributive[TracedT[F, A, ?]]
    def cobind[F[_]: Cobind, A: Semigroup] = Cobind[TracedT[F, A, ?]]
    def comonoad[F[_]: Comonad, A: Monoid] = Comonad[TracedT[F, A, ?]]
    def comonoadStore[F[_], S, A: Monoid](implicit F: ComonadStore[F, S]) = ComonadStore[TracedT[F, A, ?], S]
    def contravariant[F[_]: Functor, A] = Contravariant[TracedT[F, ?, A]]
  }

}
