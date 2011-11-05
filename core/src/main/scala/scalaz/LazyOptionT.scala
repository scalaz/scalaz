package scalaz

sealed trait LazyOptionT[F[_], A] {
  def runT: F[LazyOption[A]]

  import LazyOption._
  import LazyOptionT._
  import LazyEitherT._
  import EitherT._
  import Isomorphism.<~>

  def run(implicit i: F <~> Id): LazyOption[A] =
    i.to(runT)

  def ?[X](some: => X, none: => X)(implicit F: Functor[F]): F[X] =
    F.map(runT)(_.?(some, none))

  def -?-[X](some: => X, none: => X)(implicit i: F <~> Id): X =
    run ? (some, none)

  def isDefinedT(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_.isDefined)

  def isDefined(implicit i: F <~> Id): Boolean =
    run.isDefined

  def isEmptyT(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_.isEmpty)

  def isEmpty(implicit i: F <~> Id): Boolean =
    run.isEmpty

  def getOrElseT(default: => A)(implicit F: Functor[F]): F[A] =
    F.map(runT)(_.getOrElse(default))

  def getOrElse(default: => A)(implicit i: F <~> Id): A =
    run.getOrElse(default)

  def existsT(f: (=> A) => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_.exists(f))

  def exists(f: (=> A) => Boolean)(implicit i: F <~> Id): Boolean =
    run.exists(f)

  def forallT(f: (=> A) => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_.forall(f))

  def forall(f: (=> A) => Boolean)(implicit i: F <~> Id): Boolean =
    run.forall(f)

  def toOptionT(implicit F: Functor[F]): OptionT[F, A] =
    OptionT.optionT(F.map(runT)(_.toOption))

  def toOption(implicit i: F <~> Id): Option[A] =
    run.toOption

  def toLazyRightT[X](left: => X)(implicit F: Functor[F]): LazyEitherT[F, X, A] =
    lazyEitherT(F.map(runT)(_.toLazyRight(left)))

  def toLazyRight[X](left: => X)(implicit i: F <~> Id): LazyEither[X, A] =
    run.toLazyRight(left)

  def toLazyLeftT[X](right: => X)(implicit F: Functor[F]): LazyEitherT[F, A, X] =
    lazyEitherT(F.map(runT)(_.toLazyLeft(right)))

  def toLazyLeft[X](right: => X)(implicit i: F <~> Id): LazyEither[A, X] =
    run.toLazyLeft(right)

  def toRightT[X](left: => X)(implicit F: Functor[F]): EitherT[F, X, A] =
    eitherT(F.map(runT)(_.toRight(left)))

  def toRight[X](left: => X)(implicit i: F <~> Id): Either[X, A] =
    run.toRight(left)

  def toLeftT[X](right: => X)(implicit F: Functor[F]): EitherT[F, A, X] =
    eitherT(F.map(runT)(_.toLeft(right)))

  def toLeft[X](right: => X)(implicit i: F <~> Id): Either[A, X] =
    run.toLeft(right)

  def orElseT(a: => LazyOption[A])(implicit F: Functor[F]): LazyOptionT[F, A] =
    lazyOptionT(F.map(LazyOptionT.this.runT)(_.orElse(a)))

  def orElse(a: => LazyOption[A])(implicit i: F <~> Id): LazyOption[A] =
    run.orElse(a)

  def map[B](f: (=> A) => B)(implicit F: Functor[F]): LazyOptionT[F, B] =
    lazyOptionT(F.map(runT)(_ map f))

  def foreach(f: (=> A) => Unit)(implicit e: Each[F]): Unit =
    e.each(runT)(_ foreach f)

  def filter(f: (=> A) => Boolean)(implicit F: Functor[F]): LazyOptionT[F, A] =
    lazyOptionT(F.map(runT)(_.filter(f)))

  def flatMap[B](f: (=> A) => LazyOptionT[F, B])(implicit M: Monad[F]): LazyOptionT[F, B] =
    lazyOptionT(M.bind(runT)(_.fold(a => f(a).runT, M.point(lazyNone[B]))))

  def mapLazyOption[B](f: LazyOption[A] => LazyOption[B])(implicit F: Functor[F]): LazyOptionT[F, B] =
    lazyOptionT(F.map(runT)(f))

}

object LazyOptionT extends LazyOptionTs {
  def apply[F[_], A](r: F[LazyOption[A]]): LazyOptionT[F, A] =
    lazyOptionT(r)
}

trait LazyOptionTs {
  def lazyOptionT[F[_], A](r: F[LazyOption[A]]): LazyOptionT[F, A] = new LazyOptionT[F, A] {
    val runT = r
  }

  import LazyOption._

  def lazySomeT[F[_], A](a: => A)(implicit F: Pointed[F]): LazyOptionT[F, A] =
    lazyOptionT(F.point(lazySome(a)))

  def lazyNoneT[F[_], A](implicit F: Pointed[F]): LazyOptionT[F, A] =
    lazyOptionT(F.point(lazyNone[A]))
}