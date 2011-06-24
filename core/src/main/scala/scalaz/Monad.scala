package scalaz

trait Monad[F[_]] {
  val bind: Bind[F]
  val pointed: Pointed[F]
  val functor: Functor[F]
  val join: Join[F]

  import Monad._

  def pointedFunctor: PointedFunctor[F] = new PointedFunctor[F] {
    val functor = Monad.this.functor
    val pointed = Monad.this.pointed
  }

  def applic: Applic[F] = new Applic[F] {
    def applic[A, B](f: F[A => B]) =
      a => bind.bind[A => B, B](ff => functor.fmap(ff)(a))(f)
  }

  def applicFunctor: ApplicFunctor[F] = new ApplicFunctor[F] {
    val applic = Monad.this.applic
    val functor = Monad.this.functor
  }

  def applicative: Applicative[F] = new Applicative[F] {
    val pointedFunctor = Monad.this.pointedFunctor
    val applic = Monad.this.applic
  }

  def bindFunctor: BindFunctor[F] = new BindFunctor[F] {
    val bind = Monad.this.bind
    val functor = Monad.this.functor
  }

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def apply[A, B](f: F[A => B]): F[A] => F[B] =
    applic.applic(f)

  def bd[A, B](f: A => F[B]): F[A] => F[B] =
    bind.bind(f)

  def jn[A]: F[F[A]] => F[A] =
    join.join[A]

  def point[A](a: => A): F[A] =
    pointed.point(a)

  def liftM2[A, B, C](f: A => B => C): F[A] => F[B] => F[C] =
    a => b =>
      bd((aa: A) => fmap((bb: B) => f(aa)(bb))(b))(a)

  def deriving[G[_]](implicit n: ^**^[G, F]): Monad[G] = {
    implicit val b: Bind[G] = bind.deriving[G]
    implicit val p: Pointed[G] = pointed.deriving[G]
    monadBP[G]
  }

}

object Monad extends Monads

trait Monads {
  def monad[F[_]](implicit b: Bind[F], j: Join[F], p: PointedFunctor[F]): Monad[F] = new Monad[F] {
    val bind = b
    val pointed = p.pointed
    val functor = p.functor
    val join = j
  }

  def monadBP[F[_]](implicit b: Bind[F], p: Pointed[F]): Monad[F] = new Monad[F] {
    val bind = b
    val pointed = p
    val functor = new Functor[F] {
      def fmap[A, B](f: A => B): F[A] => F[B] = b.bind(a => p.point(f(a)))
    }
    val join = new Join[F] {
      def join[A] = b.bind(identity[F[A]])
    }
  }

  def monadJP[F[_]](implicit j: Join[F], p: PointedFunctor[F]): Monad[F] = new Monad[F] {
    val bind = new Bind[F] {
      def bind[A, B](f: A => F[B]): F[A] => F[B] =
        a => j.join(p.fmap(f)(a))
    }
    val pointed = p.pointed
    val functor = p.functor
    val join = j
  }

  implicit val OptionMonad: Monad[Option] =
    monadBP

  implicit val ListMonad: Monad[List] =
    monadBP

  implicit val StreamMonad: Monad[Stream] =
    monadBP

  implicit def EitherLeftMonad[X] =
    monadBP[({type λ[α]=Either.LeftProjection[α, X]})#λ]

  implicit def EitherRightMonad[X] =
    monadBP[({type λ[α]=Either.RightProjection[X, α]})#λ]

  implicit def EitherMonad[X] =
    monadBP[({type λ[α]=Either[X, α]})#λ]

  import java.util.Map.Entry

  implicit def MapEntryMonad[X: Monoid] = {
    implicit val z = implicitly[Monoid[X]].zero
    implicit val s = implicitly[Monoid[X]].semigroup
    monadBP[({type λ[α]=Entry[X, α]})#λ]
  }

  implicit def Tuple1Monad = {
    monadBP[Tuple1]
  }

  implicit def Tuple2Monad[R: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    monadBP[({type λ[α] = (R, α)})#λ]
  }

  implicit def Tuple3Monad[R: Monoid, S: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    monadBP[({type λ[α] = (R, S, α)})#λ]
  }

  implicit def Tuple4Monad[R: Monoid, S: Monoid, T: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    implicit val zt = implicitly[Monoid[T]].zero
    implicit val st = implicitly[Monoid[T]].semigroup
    monadBP[({type λ[α] = (R, S, T, α)})#λ]
  }

  implicit def Tuple5Monad[R: Monoid, S: Monoid, T: Monoid, U: Monoid] = {
    implicit val zr = implicitly[Monoid[R]].zero
    implicit val sr = implicitly[Monoid[R]].semigroup
    implicit val zs = implicitly[Monoid[S]].zero
    implicit val ss = implicitly[Monoid[S]].semigroup
    implicit val zt = implicitly[Monoid[T]].zero
    implicit val st = implicitly[Monoid[T]].semigroup
    implicit val zu = implicitly[Monoid[U]].zero
    implicit val su = implicitly[Monoid[U]].semigroup
    monadBP[({type λ[α] = (R, S, T, U, α)})#λ]
  }

  implicit def Tuple6Monad[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid] = {
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
    monadBP[({type λ[α] = (R, S, T, U, V, α)})#λ]
  }

  implicit def Tuple7Monad[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid] = {
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
    monadBP[({type λ[α] = (R, S, T, U, V, W, α)})#λ]
  }

  implicit def Function0Monad: Monad[Function0] =
    monadBP[Function0]

  implicit def Function1Monad[R]: Monad[({type λ[α]=(R) => α})#λ] =
    monadBP[({type λ[α]=(R) => α})#λ]

  implicit def Function2Monad[R, S]: Monad[({type λ[α]=(R, S) => α})#λ] =
    monadBP[({type λ[α]=(R, S) => α})#λ]

  implicit def Function3Monad[R, S, T]: Monad[({type λ[α]=(R, S, T) => α})#λ] =
    monadBP[({type λ[α]=(R, S, T) => α})#λ]

  implicit def Function4Monad[R, S, T, U]: Monad[({type λ[α]=(R, S, T, U) => α})#λ] =
    monadBP[({type λ[α]=(R, S, T, U) => α})#λ]

  implicit def Function5Monad[R, S, T, U, V]: Monad[({type λ[α]=(R, S, T, U, V) => α})#λ] =
    monadBP[({type λ[α]=(R, S, T, U, V) => α})#λ]

  implicit def Function6Monad[R, S, T, U, V, W]: Monad[({type λ[α]=(R, S, T, U, V, W) => α})#λ] =
    monadBP[({type λ[α]=(R, S, T, U, V, W) => α})#λ]

  implicit val IdentityMonad: Monad[Identity] =
    Monad.monadBP[Identity]

  implicit def KleisliMonad[F[_], R](implicit m: Monad[F]): Monad[({type λ[α] = Kleisli[R, F, α]})#λ] = {
    implicit val b = m.bind
    implicit val p = m.pointed
    Monad.monadBP[({type λ[α] = Kleisli[R, F, α]})#λ]
  }

  implicit val NonEmptyListMonad: Monad[NonEmptyList] =
    Monad.monadBP

  implicit def StateTMonad[A, F[_] : Monad]: Monad[({type λ[α] = StateT[A, F, α]})#λ] = {
    implicit val bind = implicitly[Monad[F]].bind
    implicit val pointed = implicitly[Monad[F]].pointed
    Monad.monadBP[({type λ[α] = StateT[A, F, α]})#λ]
  }

}
