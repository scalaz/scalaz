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
trait BindRec[F[_]] extends Bind[F] { self =>
  ////

  def tailrecM[A, B](f: A => F[A \/ B])(a: A): F[B]

  override def forever[A, B](fa: F[A]): F[B] =
    tailrecM[Unit, B](u => map(fa)(_ => -\/(u)))(())

  /**The product of BindRec `F` and `G`, `[x](F[x], G[x]])`, is a BindRec */
  def product[G[_]](implicit G0: BindRec[G]): BindRec[λ[α => (F[α], G[α])]] =
    new ProductBindRec[F, G] {
      def F = self
      def G = G0
    }

  trait BindRecLaw extends BindLaw {
    def tailrecBindConsistency[A](a: A, f: A => F[A])(implicit FA: Equal[F[A]]): Boolean = {
      val bounce = tailrecM[(Boolean, A), A] {
        case (bounced, a0) =>
          if (!bounced)
            map(f(a0))(a1 => -\/((true, a1)))
          else
            map(f(a0))(\/.right)
      }((false, a))

      FA.equal(bind(f(a))(f), bounce)
    }
  }
  def bindRecLaw = new BindRecLaw {}

  ////
  val bindRecSyntax = new scalaz.syntax.BindRecSyntax[F] { def F = BindRec.this }
}

object BindRec {
  @inline def apply[F[_]](implicit F: BindRec[F]): BindRec[F] = F

  ////

  ////
}
