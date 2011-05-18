package scalaz


trait MonadEmpty[F[_]] {
  val monad: Monad[F]
  val empty: Empty[F]

  def bind: Bind[F] = new Bind[F] {
    def bind[A, B](f: A => F[B]): F[A] => F[B] =
      monad.bd(f)
  }

  def pointed: Pointed[F] = new Pointed[F] {
    def point[A](a: => A): F[A] =
      monad.point(a)
  }

  def functor: Functor[F] = new Functor[F] {
    def fmap[A, B](f: A => B): F[A] => F[B] =
      monad.fmap(f)
  }

  def pointedFunctor: PointedFunctor[F] =
    monad.pointedFunctor

  def applic: Applic[F] =
    monad.applic

  def applicative: Applicative[F] =
    monad.applicative

  def fmap[A, B](f: A => B): F[A] => F[B] =
    monad.fmap(f)

  def apply[A, B](f: F[A => B]): F[A] => F[B] =
    monad.apply(f)

  def bd[A, B](f: A => F[B]): F[A] => F[B] =
    monad.bd(f)

  def jn[A]: F[F[A]] => F[A] =
    monad.jn[A]

  def point[A](a: => A): F[A] =
    monad.point(a)

  def e[A]: F[A] =
    empty.empty[A]
}

object MonadEmpty extends MonadEmptys

trait MonadEmptys {
  def monadEmpty[F[_]](implicit m: Monad[F], ee: Empty[F]): MonadEmpty[F] = new MonadEmpty[F] {
    val monad = m
    val empty = ee
  }

  implicit val OptionMonadEmpty: MonadEmpty[Option] =
    monadEmpty[Option]

  implicit val ListMonadEmpty: MonadEmpty[List] =
    monadEmpty[List]

  implicit val StreamMonadEmpty: MonadEmpty[Stream] =
    monadEmpty[Stream]
}