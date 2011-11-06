package scalaz

trait Semigroup[F]  { self =>
  ////

  def append(f1: F, f2: => F): F

  // derived functions

  ////
  val semigroupSyntax = new scalaz.syntax.SemigroupSyntax[F] {}
}

object Semigroup {
  @inline def apply[F](implicit F: Semigroup[F]): Semigroup[F] = F

  ////
  def firstSemigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = f1
  }

  def lastSemigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = f2
  }

  def repeat[F[_], A](a: A)(implicit F: Pointed[F], m: Semigroup[F[A]]): F[A] =
    m.append(F.point(a), repeat[F, A](a))

  def iterate[F[_], A](a: A)(f: A => A)(implicit F: Pointed[F], m: Semigroup[F[A]]): F[A] =
    m.append(F.point(a), iterate[F, A](f(a))(f))

  ////
}

