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

  implicit def EitherLeftApplicFunctor[X] =
    applicFunctor[({type λ[α] = Either.LeftProjection[α, X]})#λ]

  implicit def EitherRightApplicFunctor[X] =
    applicFunctor[({type λ[α] = Either.RightProjection[X, α]})#λ]

  implicit def EitherApplicFunctor[X] =
    applicFunctor[({type λ[α] = Either[X, α]})#λ]

  import java.util.Map.Entry

  implicit def MapEntryApplicFunctor[X: Semigroup] =
    applicFunctor[({type λ[α] = Entry[X, α]})#λ]

  implicit def Tuple1ApplicFunctor =
    applicFunctor[Tuple1]

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

  implicit def Function1ApplicFunctor[R]: ApplicFunctor[({type λ[α] = (R) => α})#λ] =
    applicFunctor[({type λ[α] = (R) => α})#λ]

  implicit def Function2ApplicFunctor[R, S]: ApplicFunctor[({type λ[α] = (R, S) => α})#λ] =
    applicFunctor[({type λ[α] = (R, S) => α})#λ]

  implicit def Function3ApplicFunctor[R, S, T]: ApplicFunctor[({type λ[α] = (R, S, T) => α})#λ] =
    applicFunctor[({type λ[α] = (R, S, T) => α})#λ]

  implicit def Function4ApplicFunctor[R, S, T, U]: ApplicFunctor[({type λ[α] = (R, S, T, U) => α})#λ] =
    applicFunctor[({type λ[α] = (R, S, T, U) => α})#λ]

  implicit def Function5ApplicFunctor[R, S, T, U, V]: ApplicFunctor[({type λ[α] = (R, S, T, U, V) => α})#λ] =
    applicFunctor[({type λ[α] = (R, S, T, U, V) => α})#λ]

  implicit def Function6ApplicFunctor[R, S, T, U, V, W]: ApplicFunctor[({type λ[α] = (R, S, T, U, V, W) => α})#λ] =
    applicFunctor[({type λ[α] = (R, S, T, U, V, W) => α})#λ]

  implicit def ConstApplicFunctor[X: Semigroup]: ApplicFunctor[({type λ[α] = Const[X, α]})#λ] =
    ApplicFunctor.applicFunctor[({type λ[α] = Const[X, α]})#λ]

  implicit val IdentityApplicFunctor: ApplicFunctor[Identity] = implicitly[Monad[Identity]].applicFunctor

  implicit def KleisliApplicFunctor[F[_], R](implicit ap: ApplicFunctor[F]): ApplicFunctor[({type λ[α] = Kleisli[R, F, α]})#λ] = {
    implicit val a = ap.applic
    implicit val f = ap.functor
    ApplicFunctor.applicFunctor[({type λ[α] = Kleisli[R, F, α]})#λ]
  }

  implicit val NonEmptyListApplicFunctor: ApplicFunctor[NonEmptyList] =
    ApplicFunctor.applicFunctor

  implicit def StateTApplicFunctor[A, F[_]](implicit m: Monad[F]): ApplicFunctor[({type λ[α] = StateT[A, F, α]})#λ] = {
    implicit val a = m.applic
    implicit val f = m.functor
    ApplicFunctor.applicFunctor[({type λ[α] = StateT[A, F, α]})#λ]
  }

  implicit val TreeApplicFunctor: ApplicFunctor[Tree] =
    ApplicFunctor.applicFunctor[Tree]

  implicit def FailProjectionApplicFunctor[X]: ApplicFunctor[({type λ[α] = FailProjection[α, X]})#λ] =
    ApplicFunctor.applicFunctor[({type λ[α] = FailProjection[α, X]})#λ]

  implicit def ValidationApplicFunctor[X: Semigroup]: ApplicFunctor[({type λ[α] = Validation[X, α]})#λ] =
    ApplicFunctor.applicFunctor[({type λ[α] = Validation[X, α]})#λ]

  implicit def WriterTApplicFunctor[A, F[_]](implicit ap: ApplicFunctor[F], n: Semigroup[A]): ApplicFunctor[({type λ[α] = WriterT[A, F, α]})#λ] = {
    implicit val a = ap.applic
    implicit val f = ap.functor
    ApplicFunctor.applicFunctor[({type λ[α] = WriterT[A, F, α]})#λ]
  }

}