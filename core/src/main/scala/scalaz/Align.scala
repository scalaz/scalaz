package scalaz

////
/**
 *
 */
////
trait Align[F[_]] extends Functor[F] { self =>
  ////

  import \&/._

  def alignWith[A, B, C](f: A \&/ B => C): (F[A], F[B]) => F[C]

  def align[A, B](a: F[A], b: F[B]): F[A \&/ B] =
    alignWith[A, B, A \&/ B](identity)(a, b)

  def padWith[A, B, C](f: (Option[A], Option[B]) => C): (F[A], F[B]) => F[C] =
    alignWith(t => {
      val (a, b) = t.pad
      f(a, b)
    })

  def pad[A, B]: (F[A], F[B]) => F[(Option[A], Option[B])] =
    padWith((a, b) => (a, b))

  def merge[A](a1: F[A], a2: F[A])(implicit S: Semigroup[A]): F[A] =
    alignWith[A, A, A](t => \&/.merge(t))(a1, a2)

  def alignSwap[A, B](a: F[A], b: F[B]): F[B \&/ A] =
    map(align(a, b))(_.swap)

  def alignA[A, B](a: F[A], b: F[B]): F[Option[A]] =
    map(align(a, b))(_.a)

  def alignB[A, B](a: F[A], b: F[B]): F[Option[B]] =
   map(align(a, b))(_.b)

  def alignThis[A, B](a: F[A], b: F[B]): F[Option[A]] =
    map(align(a, b))(_.onlyThis)

  def alignThat[A, B](a: F[A], b: F[B]): F[Option[B]] =
    map(align(a, b))(_.onlyThat)

  def alignBoth[A, B](a: F[A], b: F[B]): F[Option[(A, B)]] =
    map(align(a, b))(_.onlyBoth)

  trait AlignLaw extends FunctorLaw {
    def collapse[A](a: F[A])(implicit E: Equal[F[A \&/ A]]): Boolean =
      E.equal(map(a)(x => Both(x, x): A \&/ A), align(a, a))
  }

  def alignLaw = new AlignLaw {}

  ////
  val alignSyntax = new scalaz.syntax.AlignSyntax[F] { def F = Align.this }
}

object Align {
  @inline def apply[F[_]](implicit F: Align[F]): Align[F] = F

  ////

  ////
}
