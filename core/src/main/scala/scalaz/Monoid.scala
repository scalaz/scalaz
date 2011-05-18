package scalaz

trait Monoid[A] {
  val zero: Zero[A]
  val semigroup: Semigroup[A]

  def z: A =
    zero.zero

  def append(a1: A, a2: => A): A =
    semigroup.append(a1, a2)
}

object Monoid extends Monoids

trait Monoids {
  def monoid[A](implicit zz: Zero[A], s: Semigroup[A]): Monoid[A] = new Monoid[A] {
    val zero = zz
    val semigroup = s
  }

  implicit val UnitMonoid: Monoid[Unit] =
    monoid

  implicit val BooleanMonoid: Monoid[Boolean] =
    monoid

  implicit val StringMonoid: Monoid[String] =
    monoid

  implicit def StreamMonoid[A]: Monoid[Stream[A]] =
    monoid

  implicit def ListMonoid[A]: Monoid[List[A]] =
    monoid

  implicit def Tuple2Monoid[A, B](implicit ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = {
    implicit val sa = implicitly[Monoid[(A, B)]].semigroup
    implicit val za = implicitly[Monoid[(A, B)]].zero
    monoid[(A, B)]
  }

  implicit def Tuple3Monoid[A, B, C](implicit ma: Monoid[A], mb: Monoid[B], mc: Monoid[C]): Monoid[(A, B, C)] = {
    implicit val sa = implicitly[Monoid[(A, B, C)]].semigroup
    implicit val za = implicitly[Monoid[(A, B, C)]].zero
    monoid[(A, B, C)]
  }

  /**A monoid for sequencing Applicative effects. */
  def liftMonoid[F[_], M](implicit m: Monoid[M], a: Applicative[F]): Monoid[F[M]] = new Monoid[F[M]] {
    val zero = new Zero[F[M]] {
      val zero =
        a.point(m.z)
    }
    val semigroup: Semigroup[F[M]] = new Semigroup[F[M]] {
      def append(x: F[M], y: => F[M]) =
        a.liftA2[M, M, M](m1 => m2 => m.append(m1, m2))(x)(y)
    }
  }
}
