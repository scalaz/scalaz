package scalaz

trait Applicative[F[_]] {
  val pointedFunctor: PointedFunctor[F]
  val applic: Applic[F]

  import Applicative._

  def compose[G[_]](ga: Applicative[G]): Applicative[({type λ[α] = F[G[α]]})#λ] =
    applicativePA[({type λ[α] = F[G[α]]})#λ](
      new Pointed[({type λ[α] = F[G[α]]})#λ] {
        def point[A](a: => A) =
          pointedFunctor.point(ga.point(a))
      }
      , new Applic[({type λ[α] = F[G[α]]})#λ] {
        def applic[A, B](f: F[G[A => B]]) =
          liftA2((ff: G[A => B]) => ga.apply(ff))(f)
      }
    )

  def **[G[_] : Applicative]: Applicative[({type λ[α] = (F[α], G[α])})#λ] = {
    implicit val f = pointedFunctor ** implicitly[Applicative[G]].pointedFunctor
    implicit val a = applic ** implicitly[Applicative[G]].applic
    applicative[({type λ[α] = (F[α], G[α])})#λ]
  }

  def functor: Functor[F] = new Functor[F] {
    def fmap[A, B](f: A => B) = pointedFunctor fmap f
  }

  def pointed: Pointed[F] = new Pointed[F] {
    def point[A](a: => A) = pointedFunctor point a
  }

  def applicFunctor: ApplicFunctor[F] = new ApplicFunctor[F] {
    val applic = Applicative.this.applic
    val functor = pointedFunctor.functor
  }

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def point[A](a: => A): F[A] =
    pointed.point(a)

  def apply[A, B](f: F[A => B]): F[A] => F[B] =
    applic.applic(f)

  def liftA2[A, B, C](f: A => B => C): F[A] => F[B] => F[C] =
    a => applic.applic(pointedFunctor.fmap(f)(a))

  def deriving[G[_]](implicit n: ^**^[G, F]): Applicative[G] = {
    implicit val p: PointedFunctor[G] = pointedFunctor.deriving[G]
    implicit val a: Applic[G] = applic.deriving[G]
    applicative[G]
  }

}

object Applicative extends Applicatives

trait Applicatives {
  def applicative[F[_]](implicit p: PointedFunctor[F], a: Applic[F]): Applicative[F] = new Applicative[F] {
    val pointedFunctor = p
    val applic = a
  }

  def applicativePA[F[_]](implicit p: Pointed[F], a: Applic[F]): Applicative[F] = new Applicative[F] {
    val pointedFunctor = new PointedFunctor[F] {
      val functor = new Functor[F] {
        def fmap[A, B](f: A => B) =
          a.applic(p.point(f))
      }
      val pointed = p
    }
    val applic = a
  }

  implicit val OptionApplicative: Applicative[Option] =
    applicative[Option]

  implicit val ListApplicative: Applicative[List] =
    applicative[List]

  implicit val StreamApplicative: Applicative[Stream] =
    applicative[Stream]

  implicit def Tuple2Applicative[R: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    applicative[({type λ[α] = (R, α)})#λ]
  }

  implicit def Tuple3Applicative[R: Monoid, S: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    applicative[({type λ[α] = (R, S, α)})#λ]
  }

  implicit def Tuple4Applicative[R: Monoid, S: Monoid, T: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    implicit val zt = implicitly[Monoid[T]].zero
    implicit val st = implicitly[Monoid[T]].semigroup
    applicative[({type λ[α] = (R, S, T, α)})#λ]
  }

  implicit def Tuple5Applicative[R: Monoid, S: Monoid, T: Monoid, U: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    implicit val zt = implicitly[Monoid[T]].zero
    implicit val st = implicitly[Monoid[T]].semigroup
    implicit val zu = implicitly[Monoid[U]].zero
    implicit val su = implicitly[Monoid[U]].semigroup
    applicative[({type λ[α] = (R, S, T, U, α)})#λ]
  }

  implicit def Tuple6Applicative[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    implicit val zt = implicitly[Monoid[T]].zero
    implicit val st = implicitly[Monoid[T]].semigroup
    implicit val zu = implicitly[Monoid[U]].zero
    implicit val su = implicitly[Monoid[U]].semigroup
    implicit val zv = implicitly[Monoid[V]].zero
    implicit val sv = implicitly[Monoid[V]].semigroup
    applicative[({type λ[α] = (R, S, T, U, V, α)})#λ]
  }

  implicit def Tuple7Applicative[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    implicit val zt = implicitly[Monoid[T]].zero
    implicit val st = implicitly[Monoid[T]].semigroup
    implicit val zu = implicitly[Monoid[U]].zero
    implicit val su = implicitly[Monoid[U]].semigroup
    implicit val zv = implicitly[Monoid[V]].zero
    implicit val sv = implicitly[Monoid[V]].semigroup
    implicit val zw = implicitly[Monoid[W]].zero
    implicit val sw = implicitly[Monoid[W]].semigroup
    applicative[({type λ[α] = (R, S, T, U, V, W, α)})#λ]
  }

}
