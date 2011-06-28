package scalaz

sealed trait LazyOption[A] {

  import LazyOption._
  import LazyEither._
  import *._
  import newtypes.{FirstLazyOption, LastLazyOption}

  def fold[X](some: (=> A) => X, none: => X): X =
    this match {
      case LazySome(z) => some(z())
      case LazyNone() => none
    }

  def ?[X](some: => X, none: => X): X =
    fold(_ => some, none)

  def isDefined =
    fold(_ => false, true)

  def isEmpty =
    !isDefined

  def getOrElse(default: => A): A =
    fold(a => a, default)

  def exists(f: (=> A) => Boolean): Boolean =
    fold(f, false)

  def forall(f: (=> A) => Boolean): Boolean =
    fold(f, true)

  def toOption: Option[A] =
    fold(a => Some(a), None)

  def toLazyRight[X](left: => X): LazyEither[X, A] =
    fold(lazyRight(_), lazyLeft(left))

  def toLazyLeft[X](right: => X): LazyEither[A, X] =
    fold(lazyLeft(_), lazyRight(right))

  def toRight[X](left: => X): Either[X, A] =
    fold(Right(_), Left(left))

  def toLeft[X](right: => X): Either[A, X] =
    fold(Left(_), Right(right))

  def toList: List[A] =
    fold(List(_), Nil)

  def orElse(a: => LazyOption[A]): LazyOption[A] =
    fold(_ => this, a)

  def first: FirstLazyOption[A] =
    this.*-->[FirstLazyOption[A]]

  def last: LastLazyOption[A] =
    this.*-->[LastLazyOption[A]]

  def map[B](f: (=> A) => B): LazyOption[B] =
    fold(a => lazySome(f(a)), lazyNone)

  def foreach(f: (=> A) => Unit): Unit =
    fold(f, ())

  def filter(f: (=> A) => Boolean): LazyOption[A] =
    fold(a => if (f(a)) this else lazyNone, lazyNone)

  def flatMap[B](f: (=> A) => LazyOption[B]): LazyOption[B] =
    fold(f, lazyNone)
}

private case class LazySome[A](a: () => A) extends LazyOption[A]

private case class LazyNone[A]() extends LazyOption[A]

object LazyOption extends LazyOptions

trait LazyOptions {
  def lazySome[A]: (=> A) =>LazyOption[A] =
    a => LazySome(() => a)

  def lazyNone[A]: LazyOption[A] =
    LazyNone()

  implicit def LazyOptionShow[A: Show]: Show[LazyOption[A]] =
    Show.shows(_ map (implicitly[Show[A]].shows(_)) fold ("~Some(" + _ + ")", "~None"))

  implicit def LazyOptionEqual[A: Equal]: Equal[LazyOption[A]] =
    Equal.equalBy(_.toOption)

  implicit def LazyOptionOrder[A: Order]: Order[LazyOption[A]] =
    Order.orderBy(_.toOption)
}

sealed trait LazyOptionT[F[_], A] {
  val runT: F[LazyOption[A]]

  import LazyOption._
  import LazyOptionT._
  import LazyEitherT._
  import EitherT._

  def *->* : (({type λ[α] = LazyOptionT[F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = LazyOptionT[F, α]})#λ, A](this)

  def run(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): LazyOption[A] =
    runT.value

  def ?[X](some: => X, none: => X)(implicit ftr: Functor[F]): F[X] =
    ftr.fmap((_: LazyOption[A]).?(some, none))(runT)

  def -?-[X](some: => X, none: => X)(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): X =
    run ? (some, none)

  def isDefinedT(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: LazyOption[A]).isDefined)(runT)

  def isDefined(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): Boolean =
    run.isDefined

  def isEmptyT(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: LazyOption[A]).isEmpty)(runT)

  def isEmpty(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): Boolean =
    run.isEmpty

  def getOrElseT(default: => A)(implicit ftr: Functor[F]): F[A] =
    ftr.fmap((_: LazyOption[A]).getOrElse(default))(runT)

  def getOrElse(default: => A)(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): A =
    run.getOrElse(default)

  def existsT(f: (=> A) => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: LazyOption[A]).exists(f))(runT)

  def exists(f: (=> A) => Boolean)(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): Boolean =
    run.exists(f)

  def forallT(f: (=> A) => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: LazyOption[A]).forall(f))(runT)

  def forall(f: (=> A) => Boolean)(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): Boolean =
    run.forall(f)

  def toOptionT(implicit ftr: Functor[F]): OptionT[F, A] =
    OptionT.optionT(ftr.fmap((_: LazyOption[A]).toOption)(runT))

  def toOption(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): Option[A] =
    run.toOption

  def toLazyRightT[X](left: => X)(implicit ftr: Functor[F]): LazyEitherT[X, F, A] =
    lazyEitherT(ftr.fmap((_: LazyOption[A]).toLazyRight(left))(runT))

  def toLazyRight[X](left: => X)(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): LazyEither[X, A] =
    run.toLazyRight(left)

  def toLazyLeftT[X](right: => X)(implicit ftr: Functor[F]): LazyEitherT[A, F, X] =
    lazyEitherT(ftr.fmap((_: LazyOption[A]).toLazyLeft(right))(runT))

  def toLazyLeft[X](right: => X)(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): LazyEither[A, X] =
    run.toLazyLeft(right)

  def toRightT[X](left: => X)(implicit ftr: Functor[F]): EitherT[X, F, A] =
    eitherT(ftr.fmap((_: LazyOption[A]).toRight(left))(runT))

  def toRight[X](left: => X)(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): Either[X, A] =
    run.toRight(left)

  def toLeftT[X](right: => X)(implicit ftr: Functor[F]): EitherT[A, F, X] =
    eitherT(ftr.fmap((_: LazyOption[A]).toLeft(right))(runT))

  def toLeft[X](right: => X)(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): Either[A, X] =
    run.toLeft(right)

  def orElseT(a: => LazyOption[A])(implicit ftr: Functor[F]): LazyOptionT[F, A] =
    lazyOptionT(ftr.fmap((_: LazyOption[A]).orElse(a))(LazyOptionT.this.runT))

  def orElse(a: => LazyOption[A])(implicit i: F[LazyOption[A]] =:= Identity[LazyOption[A]]): LazyOption[A] =
    run.orElse(a)

  def map[B](f: (=> A) => B)(implicit ftr: Functor[F]): LazyOptionT[F, B] =
    lazyOptionT(ftr.fmap((_: LazyOption[A]) map f)(runT))

  def foreach(f: (=> A) => Unit)(implicit e: Each[F]): Unit =
    e.each((_: LazyOption[A]) foreach f)(runT)

  def filter(f: (=> A) => Boolean)(implicit ftr: Functor[F]): LazyOptionT[F, A] =
    lazyOptionT(ftr.fmap((_: LazyOption[A]).filter(f))(runT))

  def flatMap[B](f: (=> A) => LazyOptionT[F, B])(implicit m: Monad[F]): LazyOptionT[F, B] =
    lazyOptionT(m.bd((_: LazyOption[A]).fold(a => f(a).runT, m.point(lazyNone[B])))(runT))

  def mapLazyOption[B](f: LazyOption[A] => LazyOption[B])(implicit ftr: Functor[F]): LazyOptionT[F, B] =
    lazyOptionT(ftr.fmap(f)(runT))

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

  def lazySomeT[F[_], A](a: => A)(implicit p: Pointed[F]): LazyOptionT[F, A] =
    lazyOptionT(p.point(lazySome(a)))

  def lazyNoneT[F[_], A](implicit p: Pointed[F]): LazyOptionT[F, A] =
    lazyOptionT(p.point(lazyNone[A]))
}