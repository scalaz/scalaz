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
}
