package scalaz

object TracedTTest extends SpecLite {

  def compilationTestTracedTU: Unit = {
    import scalaz.syntax.either._
    import scalaz.std.function._

    val a: Int \/ (Byte => String) = 1.left[Byte => String]
    TracedT.tracedTU(a)
  }

  object instances {
    def functor[F[_]: Functor, A] = Functor[({type l[a] = TracedT[F, A, a]})#l]
    def apply[F[_]: Apply, A] = Apply[({type l[a] = TracedT[F, A, a]})#l]
    def applicative[F[_]: Applicative, A] = Applicative[({type l[a] = TracedT[F, A, a]})#l]
    def distributive[F[_]: Distributive, A] = Distributive[({type l[a] = TracedT[F, A, a]})#l]
    def cobind[F[_]: Cobind, A: Semigroup] = Cobind[({type l[a] = TracedT[F, A, a]})#l]
    def comonoad[F[_]: Comonad, A: Monoid] = Comonad[({type l[a] = TracedT[F, A, a]})#l]
    def contravariant[F[_]: Functor, A] = Contravariant[({type l[a] = TracedT[F, a, A]})#l]
  }

}
