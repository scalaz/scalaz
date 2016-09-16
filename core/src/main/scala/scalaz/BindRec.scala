package scalaz

////
/**
 * [[scalaz.Bind]] capable of using constant stack space when doing recursive
 * binds.
 *
 * Implementations of `tailrecM` should not make recursive calls without the
 * `@tailrec` annotation.
 *
  * Based on Phil Freeman's work on stack safety in PureScript, described in
  * [[http://functorial.com/stack-safety-for-free/index.pdf Stack Safety for
  * Free]].
 */
////
trait BindRec[F[_]] { self =>
  ////
  def bind_ : Bind[F]

  def tailrecM[A, B](a: A)(f: A => F[A \/ B]): F[B]

  def forever[A, B](fa: F[A]): F[B] =
    tailrecM[Unit, B](())(u => bind_.map(fa)(_ => -\/(u)))

  /**The product of BindRec `F` and `G`, `[x](F[x], G[x]])`, is a BindRec */
  def product[G[_]](implicit G0: BindRec[G]): BindRec[λ[α => (F[α], G[α])]] =
    new ProductBindRec[F, G] {
      def F = self
      def G = G0
    }

  trait BindRecLaw {
    def tailrecBindConsistency[A](a: A, f: A => F[A])(implicit FA: Equal[F[A]]): Boolean = {
      val bounce = tailrecM[(Boolean, A), A]((false, a)) {
        case (bounced, a0) =>
          if (!bounced)
            bind_.map(f(a0))(a1 => -\/((true, a1)))
          else
            bind_.map(f(a0))(\/.right)
      }

      FA.equal(bind_.bind(f(a))(f), bounce)
    }
  }
  def bindRecLaw = new BindRecLaw {}

  ////
  val bindRecSyntax = new scalaz.syntax.BindRecSyntax[F] { def FB = BindRec.this }
}

object BindRec {
  @inline def apply[F[_]](implicit F: BindRec[F]): BindRec[F] = F

  ////

  ////
}
