package scalaz

import scalaz.Pointed._


trait ApplicFunctor[F[_]] {
  val applic: Applic[F]
  val functor: Functor[F]

  import ApplicFunctor._

  def compose[G[_]](ga: ApplicFunctor[G]): ApplicFunctor[({type λ[α] = F[G[α]]})#λ] =
    applicFunctor[({type λ[α] = F[G[α]]})#λ](
      new Applic[({type λ[α] = F[G[α]]})#λ] {
        def applic[A, B](f: F[G[A => B]]) =
          liftA2((ff: G[A => B]) => ga.apply(ff))(f)
      }
      , new Functor[({type λ[α] = F[G[α]]})#λ] {
        def fmap[A, B](f: A => B) =
          ApplicFunctor.this.fmap(ga.fmap(f))
      }
    )

  def **[G[_] : ApplicFunctor]: ApplicFunctor[({type λ[α] = (F[α], G[α])})#λ] = {
    implicit val a = applic ** implicitly[ApplicFunctor[G]].applic
    implicit val f = functor ** implicitly[ApplicFunctor[G]].functor
    applicFunctor[({type λ[α] = (F[α], G[α])})#λ]
  }

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def apply[A, B](f: F[A => B]): F[A] => F[B] =
    applic.applic(f)

  def liftA2[A, B, C](f: A => B => C): F[A] => F[B] => F[C] =
    a => applic.applic(functor.fmap(f)(a))

  def deriving[G[_]](implicit n: ^**^[G, F]): ApplicFunctor[G] = {
    implicit val a: Applic[G] = applic.deriving[G]
    implicit val f: Functor[G] = functor.deriving[G]
    applicFunctor[G]
  }

}

object ApplicFunctor extends ApplicFunctors

trait ApplicFunctors {
  def applicFunctor[F[_]](implicit a: Applic[F], f: Functor[F]): ApplicFunctor[F] = new ApplicFunctor[F] {
    val applic = a
    val functor = f
  }

  implicit val OptionApplicFunctor: ApplicFunctor[Option] =
    applicFunctor

  implicit val ListApplicFunctor: ApplicFunctor[List] =
    applicFunctor

  implicit val StreamApplicFunctor: ApplicFunctor[Stream] =
    applicFunctor

  implicit def Tuple2ApplicFunctor[R: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    applicFunctor[({type λ[α] = (R, α)})#λ]
  }

  implicit def Tuple3ApplicFunctor[R: Monoid, S: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    applicFunctor[({type λ[α] = (R, S, α)})#λ]
  }

  implicit def Tuple4ApplicFunctor[R: Monoid, S: Monoid, T: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    implicit val zt = implicitly[Monoid[T]].zero
    implicit val st = implicitly[Monoid[T]].semigroup
    applicFunctor[({type λ[α] = (R, S, T, α)})#λ]
  }

  implicit def Tuple5ApplicFunctor[R: Monoid, S: Monoid, T: Monoid, U: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    implicit val zt = implicitly[Monoid[T]].zero
    implicit val st = implicitly[Monoid[T]].semigroup
    implicit val zu = implicitly[Monoid[U]].zero
    implicit val su = implicitly[Monoid[U]].semigroup
    applicFunctor[({type λ[α] = (R, S, T, U, α)})#λ]
  }

  implicit def Tuple6ApplicFunctor[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid] = {
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
    applicFunctor[({type λ[α] = (R, S, T, U, V, α)})#λ]
  }

  implicit def Tuple7ApplicFunctor[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid] = {
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
    applicFunctor[({type λ[α] = (R, S, T, U, V, W, α)})#λ]
  }

  implicit def Function0ApplicFunctor: ApplicFunctor[Function0] =
    applicFunctor[Function0]

  implicit def Function1ApplicFunctor[R]: ApplicFunctor[({type λ[α]=(R) => α})#λ] =
    applicFunctor[({type λ[α]=(R) => α})#λ]

  implicit def Function2ApplicFunctor[R, S]: ApplicFunctor[({type λ[α]=(R, S) => α})#λ] =
    applicFunctor[({type λ[α]=(R, S) => α})#λ]

  implicit def Function3ApplicFunctor[R, S, T]: ApplicFunctor[({type λ[α]=(R, S, T) => α})#λ] =
    applicFunctor[({type λ[α]=(R, S, T) => α})#λ]

  implicit def Function4ApplicFunctor[R, S, T, U]: ApplicFunctor[({type λ[α]=(R, S, T, U) => α})#λ] =
    applicFunctor[({type λ[α]=(R, S, T, U) => α})#λ]

  implicit def Function5ApplicFunctor[R, S, T, U, V]: ApplicFunctor[({type λ[α]=(R, S, T, U, V) => α})#λ] =
    applicFunctor[({type λ[α]=(R, S, T, U, V) => α})#λ]

  implicit def Function6ApplicFunctor[R, S, T, U, V, W]: ApplicFunctor[({type λ[α]=(R, S, T, U, V, W) => α})#λ] =
    applicFunctor[({type λ[α]=(R, S, T, U, V, W) => α})#λ]

}