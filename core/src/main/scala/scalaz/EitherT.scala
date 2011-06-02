package scalaz

sealed trait EitherT[A, F[_], B] {
  val runT: F[Either[A, B]]

  import EitherT._
  import OptionT._

  def *->* : (({type λ[α] = EitherT[A, F, α]})#λ *->* B) =
    scalaz.*->*.**->**[({type λ[α] = EitherT[A, F, α]})#λ, B](this)

  def *->*->* : *->*->*[A, ({type λ[α, β] = EitherT[α, F, β]})#λ, B] =
    scalaz.*->*->*.**->**->**[A, ({type λ[α, β] = EitherT[α, F, β]})#λ, B](this)

  def run(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Either[A, B] =
    runT.value

  def ?[X](left: => X, right: => X)(implicit ftr: Functor[F]): F[X] =
    ftr.fmap((_: Either[A, B]).fold(_ => left, _ => right))(runT)

  def -?-[X](left: => X, right: => X)(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): X =
    run.fold(_ => left, _ => right)

  def isLeftT(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: Either[A, B]).isLeft)(runT)

  def isLeft(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Boolean =
    run.isLeft

  def isRightT(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: Either[A, B]).isRight)(runT)

  def isRight(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Boolean =
    run.isRight

  def swapT(implicit ftr: Functor[F]): F[Either[B, A]] =
    ftr.fmap((_: Either[A, B]).swap: Either[B, A])(runT)

  def swap(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Either[B, A] =
    run.swap

  def getOrElseT(default: => B)(implicit ftr: Functor[F]): F[B] =
    ftr.fmap((_: Either[A, B]).right getOrElse default)(runT)

  def getOrElse(default: => B)(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): B =
    run.right getOrElse default

  def existsT(f: B => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: Either[A, B]).right exists f)(runT)

  def exists(f: B => Boolean)(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Boolean =
    run.right exists f

  def forallT(f: B => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: Either[A, B]).right forall f)(runT)

  def forall(f: B => Boolean)(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Boolean =
    run.right forall f

  def orElseT(x: => Either[A, B])(implicit ftr: Functor[F]): EitherT[A, F, B] =
    eitherT(ftr.fmap((e: Either[A, B]) => e.fold(_ => x, _ => e))(runT))

  def orElse(x: => Either[A, B])(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Either[A, B] =
    run.fold(_ => x, _ => run)

  def toOptionT(implicit ftr: Functor[F]): OptionT[F, B] =
    optionT(ftr.fmap((_: Either[A, B]).right toOption)(runT))

  def toOption(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Option[B] =
    run.right toOption

  def toListT(implicit ftr: Functor[F]): F[List[B]] =
    ftr.fmap((_: Either[A, B]).fold(_ => Nil, List(_)))(runT)

  def toList(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): List[B] =
    run.fold(_ => Nil, List(_))

  def toStreamT(implicit ftr: Functor[F]): F[Stream[B]] =
    ftr.fmap((_: Either[A, B]).fold(_ => Stream(), Stream(_)))(runT)

  def toStream(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Stream[B] =
    run.fold(_ => Stream(), Stream(_))

  def map[C](f: B => C)(implicit ftr: Functor[F]): EitherT[A, F, C] =
    eitherT(ftr.fmap((_: Either[A, B]).right.map(f): Either[A, C])(runT))

  def foreach(f: B => Unit)(implicit e: Each[F]): Unit =
    e.each((_: Either[A, B]).right foreach f)(runT)

  def flatMap[C](f: B => EitherT[A, F, C])(implicit m: Monad[F]): EitherT[A, F, C] =
    eitherT(m.bd((_: Either[A, B]).fold(a => m.point(Left(a): Either[A, C]), b => f(b).runT))(runT))

  def left = new LeftProjectionT[A, F, B]() {
    val e = EitherT.this
  }

}

object EitherT extends EitherTs {
  def apply[A, F[_], B](a: F[Either[A, B]]): EitherT[A, F, B] =
    eitherT[A, F, B](a)

  sealed trait LeftProjectionT[A, F[_], B] {
    val e: EitherT[A, F, B]

    import OptionT._

    def getOrElseT(default: => A)(implicit ftr: Functor[F]): F[A] =
      ftr.fmap((_: Either[A, B]).left getOrElse default)(e.runT)

    def getOrElse(default: => A)(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): A =
      e.run.left getOrElse default

    def existsT(f: A => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
      ftr.fmap((_: Either[A, B]).left exists f)(e.runT)

    def exists(f: A => Boolean)(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Boolean =
      e.run.left exists f

    def forallT(f: A => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
      ftr.fmap((_: Either[A, B]).left forall f)(e.runT)

    def forall(f: A => Boolean)(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Boolean =
      e.run.left forall f

    def orElseT(x: => Either[A, B])(implicit ftr: Functor[F]): EitherT[A, F, B] =
      eitherT(ftr.fmap((e: Either[A, B]) => e.fold(_ => e, _ => x))(e.runT))

    def orElse(x: => Either[A, B])(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Either[A, B] =
      e.run.fold(_ => x, _ => e.run)

    def toOptionT(implicit ftr: Functor[F]): OptionT[F, A] =
      optionT(ftr.fmap((_: Either[A, B]).left toOption)(e.runT))

    def toOption(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Option[A] =
      e.run.left toOption

    def toListT(implicit ftr: Functor[F]): F[List[A]] =
      ftr.fmap((_: Either[A, B]).fold(List(_), _ => Nil))(e.runT)

    def toList(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): List[A] =
      e.run.fold(List(_), _ => Nil)

    def toStreamT(implicit ftr: Functor[F]): F[Stream[A]] =
      ftr.fmap((_: Either[A, B]).fold(Stream(_), _ => Stream()))(e.runT)

    def toStream(implicit i: F[Either[A, B]] =:= Ident[Either[A, B]]): Stream[A] =
      e.run.fold(Stream(_), _ => Stream())

    def map[C](f: A => C)(implicit ftr: Functor[F]): EitherT[C, F, B] =
      eitherT(ftr.fmap((_: Either[A, B]).left.map(f): Either[C, B])(e.runT))

    def foreach(f: A => Unit)(implicit eh: Each[F]): Unit =
      eh.each((_: Either[A, B]).left foreach f)(e.runT)

    def flatMap[C](f: A => EitherT[C, F, B])(implicit m: Monad[F]): EitherT[C, F, B] =
      eitherT(m.bd((_: Either[A, B]).fold(a => f(a).runT, b => m.point(Right(b): Either[C, B])))(e.runT))
  }

}

trait EitherTs {
  def eitherT[A, F[_], B](a: F[Either[A, B]]): EitherT[A, F, B] = new EitherT[A, F, B] {
    val runT = a
  }

  def leftT[A, F[_], B](implicit p: Pointed[F]): A => EitherT[A, F, B] =
    a => eitherT(p.point(Left(a): Either[A, B]))

  def rightT[A, F[_], B](implicit p: Pointed[F]): B => EitherT[A, F, B] =
    b => eitherT(p.point(Right(b): Either[A, B]))

  implicit def EitherTMonadTrans[Z]: MonadTrans[({type λ[α[_], β] = EitherT[Z, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = EitherT[Z, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): EitherT[Z, G, A] =
      eitherT(implicitly[Monad[G]].fmap((a: A) => Right(a): Either[Z, A])(a))
  }
}
