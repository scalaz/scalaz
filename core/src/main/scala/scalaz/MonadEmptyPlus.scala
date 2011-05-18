package scalaz

trait MonadEmptyPlus[F[_]] {
  val monad: Monad[F]
  val empty: Empty[F]
  val plus: Plus[F]

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

  def monadEmpty: MonadEmpty[F] = new MonadEmpty[F] {
    val monad = MonadEmptyPlus.this.monad
    val empty = MonadEmptyPlus.this.empty
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

  def pl[A](a1: F[A], a2: => F[A]): F[A] =
    plus.plus(a1, a2)
}

object MonadEmptyPlus extends MonadEmptyPluss

trait MonadEmptyPluss {
  def monadEmptyPlus[F[_]](implicit m: Monad[F], ee: Empty[F], p: Plus[F]): MonadEmptyPlus[F] = new MonadEmptyPlus[F] {
    val monad = m
    val empty = ee
    val plus = p
  }

  def monadEmptyPlusMP[F[_]](implicit m: MonadEmpty[F], p: Plus[F]): MonadEmptyPlus[F] = new MonadEmptyPlus[F] {
    val monad = m.monad
    val empty = m.empty
    val plus = p
  }

  implicit val OptionMonadEmptyPlus: MonadEmptyPlus[Option] =
    monadEmptyPlusMP[Option]

  implicit val ListMonadEmptyPlus: MonadEmptyPlus[List] =
    monadEmptyPlusMP[List]

  implicit val StreamMonadEmptyPlus: MonadEmptyPlus[Stream] =
    monadEmptyPlusMP[Stream]
}