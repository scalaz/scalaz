package scalaz


sealed trait LazyEither[A, B] {

  import LazyOption._
  import LazyEither._

  def fold[X](left: (=> A) => X, right: (=> B) => X): X =
    this match {
      case LazyLeft(a) => left(a())
      case LazyRight(b) => right(b())
    }

  def *->* : (({type λ[α] = LazyEither[A, α]})#λ *->* B) =
    scalaz.*->*.!**->**![({type λ[α] = LazyEither[A, α]})#λ, B](this)

  def ?[X](left: => X, right: => X): X =
    fold(_ => left, _ => right)

  def isLeft =
    fold(_ => true, _ => false)

  def isRight =
    !isLeft

  def swap: LazyEither[B, A] =
    fold(lazyRight(_), lazyLeft(_))

  def toEither: Either[A, B] =
    fold(Left(_), Right(_))

  def getOrElse(default: => B): B =
    fold(_ => default, z => z)

  def exists(f: (=> B) => Boolean): Boolean =
    fold(_ => false, f)

  def forall(f: (=> B) => Boolean): Boolean =
    fold(_ => true, f)

  def orElse(x: => LazyEither[A, B]): LazyEither[A, B] =
    ?(x, this)

  def toLazyOption: LazyOption[B] =
    fold(_ => lazyNone, lazySome(_))

  def toOption: Option[B] =
    fold(_ => None, Some(_))

  def toList: List[B] =
    fold(_ => Nil, List(_))

  def toStream: Stream[B] =
    fold(_ => Stream(), Stream(_))

  def map[C](f: (=> B) => C): LazyEither[A, C] =
    fold(lazyLeft(_), b => lazyRight(f(b)))

  def foreach(f: (=> B) => Unit): Unit =
    fold(_ => (), f)

  def flatMap[C](f: (=> B) => LazyEither[A, C]): LazyEither[A, C] =
    fold(lazyLeft(_), f)

  def left = new LazyLeftProjection[A, B]() {
    val e = LazyEither.this
  }

}

private case class LazyLeft[A, B](a: () => A) extends LazyEither[A, B]

private case class LazyRight[A, B](b: () => B) extends LazyEither[A, B]

object LazyEither extends LazyEithers

trait LazyEithers {

  trait LazyLeftConstruct[B] {
    def apply[A](a: => A): LazyEither[A, B]
  }

  def lazyLeft[B]: LazyLeftConstruct[B] = new LazyLeftConstruct[B] {
    def apply[A](a: => A) = LazyLeft(() => a)
  }

  trait LazyRightConstruct[A] {
    def apply[B](b: => B): LazyEither[A, B]
  }

  def lazyRight[A]: LazyRightConstruct[A] = new LazyRightConstruct[A] {
    def apply[B](b: => B) = LazyRight(() => b)
  }

  sealed trait LazyLeftProjection[A, B] {
    val e: LazyEither[A, B]

    import LazyOption._

    def getOrElse(default: => A): A =
      e.fold(z => z, _ => default)

    def exists(f: (=> A) => Boolean): Boolean =
      e.fold(f, _ => false)

    def forall(f: (=> A) => Boolean): Boolean =
      e.fold(f, _ => true)

    def orElse(x: => LazyEither[A, B]): LazyEither[A, B] =
      e.?(e, x)

    def toLazyOption: LazyOption[A] =
      e.fold(lazySome(_), _ => lazyNone)

    def toOption: Option[A] =
      e.fold(Some(_), _ => None)

    def toList: List[A] =
      e.fold(List(_), _ => Nil)

    def toStream: Stream[A] =
      e.fold(Stream(_), _ => Stream())

    def map[C](f: (=> A) => C): LazyEither[C, B] =
      e.fold(a => lazyLeft(f(a)), lazyRight(_))

    def foreach(f: (=> A) => Unit): Unit =
      e.fold(f, _ => ())

    def flatMap[C](f: (=> A) => LazyEither[C, B]): LazyEither[C, B] =
      e.fold(f, lazyRight(_))
  }

}

sealed trait LazyEitherT[A, F[_], B] {
  val runT: F[LazyEither[A, B]]

  import LazyEither._
  import LazyEitherT._
  import EitherT._
  import OptionT._
  import LazyOptionT._

  def *->* : (({type λ[α] = LazyEitherT[A, F, α]})#λ *->* B) =
    scalaz.*->*.!**->**![({type λ[α] = LazyEitherT[A, F, α]})#λ, B](this)

  def *->*->* : *->*->*[A, ({type λ[α, β] = LazyEitherT[α, F, β]})#λ, B] =
    scalaz.*->*->*.!**->**->**![A, ({type λ[α, β] = LazyEitherT[α, F, β]})#λ, B](this)

  def run(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): LazyEither[A, B] =
    runT.value

  def ?[X](left: => X, right: => X)(implicit ftr: Functor[F]): F[X] =
    ftr.fmap((_: LazyEither[A, B]).fold(_ => left, _ => right))(runT)

  def -?-[X](left: => X, right: => X)(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): X =
    run ? (left, right)

  def isLeftT(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: LazyEither[A, B]).isLeft)(runT)

  def isLeft(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Boolean =
    run.isLeft

  def isRightT(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: LazyEither[A, B]).isRight)(runT)

  def isRight(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Boolean =
    run.isRight

  def swapT(implicit ftr: Functor[F]): F[LazyEither[B, A]] =
    ftr.fmap((_: LazyEither[A, B]).swap)(runT)

  def swap(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): LazyEither[B, A] =
    run.swap

  def toEitherT(implicit ftr: Functor[F]): EitherT[A, F, B] =
    eitherT(ftr.fmap((_: LazyEither[A, B]).toEither)(runT))

  def toEither(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Either[A, B] =
    run.toEither

  def getOrElseT(default: => B)(implicit ftr: Functor[F]): F[B] =
    ftr.fmap((_: LazyEither[A, B]) getOrElse default)(runT)

  def getOrElse(default: => B)(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): B =
    run getOrElse default

  def existsT(f: (=> B) => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: LazyEither[A, B]) exists f)(runT)

  def exists(f: (=> B) => Boolean)(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Boolean =
    run exists f

  def forallT(f: (=> B) => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: LazyEither[A, B]) forall f)(runT)

  def forall(f: (=> B) => Boolean)(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Boolean =
    run forall f

  def orElse(x: => LazyEitherT[A, F, B])(implicit m: Bind[F]): LazyEitherT[A, F, B] ={
    val g = runT
    LazyEitherT(m.bind((z: LazyEither[A, B]) => z.fold(
      _ => x.runT
    , _ => g
    ))(g))
  }

  def toLazyOptionT(implicit ftr: Functor[F]): LazyOptionT[F, B] =
    lazyOptionT(ftr.fmap((_: LazyEither[A, B]) toLazyOption)(runT))

  def toLazyOption(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): LazyOption[B] =
    run toLazyOption

  def toOptionT(implicit ftr: Functor[F]): OptionT[F, B] =
    optionT(ftr.fmap((_: LazyEither[A, B]) toOption)(runT))

  def toOption(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Option[B] =
    run toOption

  def toListT(implicit ftr: Functor[F]): F[List[B]] =
    ftr.fmap((_: LazyEither[A, B]) toList)(runT)

  def toList(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): List[B] =
    run toList

  def toStreamT(implicit ftr: Functor[F]): F[Stream[B]] =
    ftr.fmap((_: LazyEither[A, B]) toStream)(runT)

  def toStream(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Stream[B] =
    run toStream

  def map[C](f: (=> B) => C)(implicit ftr: Functor[F]): LazyEitherT[A, F, C] =
    lazyEitherT(ftr.fmap((_: LazyEither[A, B]) map f)(runT))

  def foreach(f: (=> B) => Unit)(implicit e: Each[F]): Unit =
    e.each((_: LazyEither[A, B]) foreach f)(runT)

  def flatMap[C](f: (=> B) => LazyEitherT[A, F, C])(implicit m: Monad[F]): LazyEitherT[A, F, C] =
    lazyEitherT(m.bd((_: LazyEither[A, B]).fold(a => m.point(lazyLeft[C](a)), b => f(b).runT))(runT))

  def left: LazyLeftProjectionT[A, F, B] = new LazyLeftProjectionT[A, F, B]() {
    val e = LazyEitherT.this
  }

}

object LazyEitherT extends LazyEitherTs {
  def apply[A, F[_], B](a: F[LazyEither[A, B]]): LazyEitherT[A, F, B] =
    lazyEitherT(a)
}

trait LazyEitherTs {
  def lazyEitherT[A, F[_], B](a: F[LazyEither[A, B]]): LazyEitherT[A, F, B] = new LazyEitherT[A, F, B] {
    val runT = a
  }

  import LazyEither._

  def lazyLeftT[A, F[_], B](a: => A)(implicit p: Pointed[F]): LazyEitherT[A, F, B] =
    lazyEitherT(p.point(lazyLeft(a)))

  def lazyRightT[A, F[_], B](b: => B)(implicit p: Pointed[F]): LazyEitherT[A, F, B] =
    lazyEitherT(p.point(lazyRight(b)))

  implicit def LazyEitherTMonadTrans[Z]: MonadTrans[({type λ[α[_], β] = LazyEitherT[Z, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = LazyEitherT[Z, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): LazyEitherT[Z, G, A] =
      lazyEitherT(implicitly[Monad[G]].fmap((a: A) => lazyRight(a): LazyEither[Z, A])(a))
  }

  sealed trait LazyLeftProjectionT[A, F[_], B] {
    val e: LazyEitherT[A, F, B]

    import OptionT._
    import LazyOptionT._

    def getOrElseT(default: => A)(implicit ftr: Functor[F]): F[A] =
      ftr.fmap((_: LazyEither[A, B]).left getOrElse default)(e.runT)

    def getOrElse(default: => A)(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): A =
      e.run.left getOrElse default

    def existsT(f: (=> A) => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
      ftr.fmap((_: LazyEither[A, B]).left exists f)(e.runT)

    def exists(f: (=> A) => Boolean)(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Boolean =
      e.run.left exists f

    def forallT(f: (=> A) => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
      ftr.fmap((_: LazyEither[A, B]).left forall f)(e.runT)

    def forall(f: (=> A) => Boolean)(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Boolean =
      e.run.left forall f

    def orElse(x: => LazyEitherT[A, F, B])(implicit m: Bind[F]): LazyEitherT[A, F, B] = {
      val g = e.runT
      LazyEitherT(m.bind((z: LazyEither[A, B]) => z.fold(
        _ => g
      , _ => x.runT
      ))(g))
    }

    def toLazyOptionT(implicit ftr: Functor[F]): LazyOptionT[F, A] =
      lazyOptionT(ftr.fmap((_: LazyEither[A, B]).left toLazyOption)(e.runT))

    def toLazyOption(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): LazyOption[A] =
      e.run.left toLazyOption

    def toOptionT(implicit ftr: Functor[F]): OptionT[F, A] =
      optionT(ftr.fmap((_: LazyEither[A, B]).left toOption)(e.runT))

    def toOption(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Option[A] =
      e.run.left toOption

    def toListT(implicit ftr: Functor[F]): F[List[A]] =
      ftr.fmap((_: LazyEither[A, B]).left toList)(e.runT)

    def toList(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): List[A] =
      e.run.left toList

    def toStreamT(implicit ftr: Functor[F]): F[Stream[A]] =
      ftr.fmap((_: LazyEither[A, B]).left toStream)(e.runT)

    def toStream(implicit i: F[LazyEither[A, B]] =:= Identity[LazyEither[A, B]]): Stream[A] =
      e.run.left toStream

    def map[C](f: (=> A) => C)(implicit ftr: Functor[F]): LazyEitherT[C, F, B] =
      lazyEitherT(ftr.fmap((_: LazyEither[A, B]).left map f)(e.runT))

    def foreach(f: (=> A) => Unit)(implicit eh: Each[F]): Unit =
      eh.each((_: LazyEither[A, B]).left foreach f)(e.runT)

    def flatMap[C](f: (=> A) => LazyEitherT[C, F, B])(implicit m: Monad[F]): LazyEitherT[C, F, B] =
      lazyEitherT(m.bd((_: LazyEither[A, B]).fold(a => f(a).runT, b => m.point(lazyRight[C](b))))(e.runT))
  }

}