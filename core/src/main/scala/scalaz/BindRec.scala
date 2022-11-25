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

  def tailrecM[A, B](a: A)(f: A => F[A \/ B]): F[B]

  override def forever[A, B](fa: F[A]): F[B] =
    tailrecM[Unit, B](())(u => map(fa)(_ => -\/(u)))

  /**The product of BindRec `F` and `G`, `[x](F[x], G[x]])`, is a BindRec */
  def product[G[_]](implicit G0: BindRec[G]): BindRec[λ[α => (F[α], G[α])]] =
    new ProductBindRec[F, G] {
      def F = self
      def G = G0
    }

  trait BindRecLaw extends BindLaw {
    def tailrecBindConsistency[A](a: A, f: A => F[A])(implicit FA: Equal[F[A]]): Boolean = {
      val g: ((Int, A)) => F[(Int, A) \/ A] = { case (i, a) =>
        if (i > 0)
          map(f(a))(a => -\/((i-1, a)))
        else
          map(f(a))(\/.right)
      }

      val result = tailrecM[(Int, A), A]((4, a))(g)
      val expected = bind(f(a))(a => bind(f(a))(a => bind(f(a))(a => bind(f(a))(f))))

      FA.equal(result, expected)
    }
  }
  def bindRecLaw: BindRecLaw = new BindRecLaw {}

  ////
  val bindRecSyntax: scalaz.syntax.BindRecSyntax[F] =
    new scalaz.syntax.BindRecSyntax[F] { def F = BindRec.this }
}

object BindRec {
  @inline def apply[F[_]](implicit F: BindRec[F]): BindRec[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: BindRec[G]): BindRec[F] =
    new IsomorphismBindRec[F, G] {
      override def G: BindRec[G] = E
      override def iso: F <~> G = D
    }

  ////
  implicit def idInstance: BindRec[Id.Id] = Id.id

  ////
}

trait IsomorphismBindRec[F[_], G[_]] extends BindRec[F] with IsomorphismBind[F, G]{
  implicit def G: BindRec[G]
  ////

  override def tailrecM[A, B](a: A)(f: A => F[A \/ B]): F[B] =
    iso.from(G.tailrecM(a)(f andThen iso.unlift[A \/ B].to))
  ////
}
