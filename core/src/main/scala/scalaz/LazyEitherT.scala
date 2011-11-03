package scalaz

sealed trait LazyEitherT[A, F[_], B] {
  def runT: F[LazyEither[A, B]]

  import LazyEither._
  import LazyEitherT._
  import EitherT._
  import OptionT._
  import LazyOptionT._
  import Isomorphism.{<~>, <~~>}

  def run(implicit i: F <~> Id): LazyEither[A, B] =
    i.to(runT)

  def ?[X](left: => X, right: => X)(implicit F: Functor[F]): F[X] =
    F.map(runT)(_.fold(_ => left, _ => right))

  def -?-[X](left: => X, right: => X)(implicit i: F <~> Id): X =
    run ?(left, right)

  def isLeftT(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_.isLeft)

  def isLeft(implicit i: F <~> Id): Boolean =
    run.isLeft

  def isRightT(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_.isRight)

  def isRight(implicit i: F <~> Id): Boolean =
    run.isRight

  def swapT(implicit F: Functor[F]): F[LazyEither[B, A]] =
    F.map(runT)(_.swap)

  def swap(implicit i: F <~> Id): LazyEither[B, A] =
    run.swap

  def toEitherT(implicit F: Functor[F]): EitherT[A, F, B] =
    eitherT(F.map(runT)(_.toEither))

  def toEither(implicit i: F <~> Id): Either[A, B] =
    run.toEither

  def getOrElseT(default: => B)(implicit F: Functor[F]): F[B] =
    F.map(runT)(_ getOrElse default)

  def getOrElse(default: => B)(implicit i: F <~> Id): B =
    run getOrElse default

  def existsT(f: (=> B) => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_ exists f)

  def exists(f: (=> B) => Boolean)(implicit i: F <~> Id): Boolean =
    run exists f

  def forallT(f: (=> B) => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(runT)(_ forall f)

  def forall(f: (=> B) => Boolean)(implicit i: F <~> Id): Boolean =
    run forall f

  def orElse(x: => LazyEitherT[A, F, B])(implicit m: Bind[F]): LazyEitherT[A, F, B] = {
    val g = runT
    LazyEitherT(m.bind(g)(_.fold(
      _ => x.runT
      , _ => g
    )))
  }

  def toLazyOptionT(implicit F: Functor[F]): LazyOptionT[F, B] =
    lazyOptionT(F.map(runT)(_ toLazyOption))

  def toLazyOption(implicit i: F <~> Id): LazyOption[B] =
    run toLazyOption

  def toOptionT(implicit F: Functor[F]): OptionT[F, B] =
    optionT(F.map(runT)(_ toOption))

  def toOption(implicit i: F <~> Id): Option[B] =
    run toOption

  def toListT(implicit F: Functor[F]): F[List[B]] =
    F.map(runT)(_ toList)

  def toList(implicit i: F <~> Id): List[B] =
    run toList

  def toStreamT(implicit F: Functor[F]): F[Stream[B]] =
    F.map(runT)(_ toStream)

  def toStream(implicit i: F <~> Id): Stream[B] =
    run toStream

  def map[C](f: (=> B) => C)(implicit F: Functor[F]): LazyEitherT[A, F, C] =
    lazyEitherT(F.map(runT)(_ map f))

  def foreach(f: (=> B) => Unit)(implicit e: Each[F]): Unit =
    e.each(runT)(_ foreach f)

  def flatMap[C](f: (=> B) => LazyEitherT[A, F, C])(implicit M: Monad[F]): LazyEitherT[A, F, C] =
    lazyEitherT(M.bind(runT)(_.fold(a => M.pure(lazyLeft[C](a)), b => f(b).runT)))

  def left: LazyLeftProjectionT[A, F, B] = new LazyLeftProjectionT[A, F, B]() {
    val e = LazyEitherT.this
  }

}

object LazyEitherT extends LazyEitherTFunctions with LazyEitherTInstances {
  def apply[A, F[_], B](a: F[LazyEither[A, B]]): LazyEitherT[A, F, B] =
    lazyEitherT(a)

  sealed trait LazyLeftProjectionT[A, F[_], B] {
    def e: LazyEitherT[A, F, B]

    import OptionT._
    import LazyOptionT._
    import Isomorphism.<~>

    def getOrElseT(default: => A)(implicit F: Functor[F]): F[A] =
      F.map(e.runT)(_.left getOrElse default)

    def getOrElse(default: => A)(implicit i: F <~> Id): A =
      e.run.left getOrElse default

    def existsT(f: (=> A) => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(e.runT)(_.left exists f)

    def exists(f: (=> A) => Boolean)(implicit i: F <~> Id): Boolean =
      e.run.left exists f

    def forallT(f: (=> A) => Boolean)(implicit F: Functor[F]): F[Boolean] =
      F.map(e.runT)(_.left forall f)

    def forall(f: (=> A) => Boolean)(implicit i: F <~> Id): Boolean =
      e.run.left forall f

    def orElse(x: => LazyEitherT[A, F, B])(implicit m: Bind[F]): LazyEitherT[A, F, B] = {
      val g = e.runT
      LazyEitherT(m.bind(g)((z: LazyEither[A, B]) => z.fold(
        _ => g
        , _ => x.runT
      )))
    }

    def toLazyOptionT(implicit F: Functor[F]): LazyOptionT[F, A] =
      lazyOptionT(F.map(e.runT)(_.left toLazyOption))

    def toLazyOption(implicit i: F <~> Id): LazyOption[A] =
      e.run.left.toLazyOption

    def toOptionT(implicit F: Functor[F]): OptionT[F, A] =
      optionT(F.map(e.runT)(_.left toOption))

    def toOption(implicit i: F <~> Id): Option[A] =
      e.run.left.toOption

    def toListT(implicit F: Functor[F]): F[List[A]] =
      F.map(e.runT)(_.left toList)

    def toList(implicit i: F <~> Id): List[A] =
      e.run.left.toList

    def toStreamT(implicit F: Functor[F]): F[Stream[A]] =
      F.map(e.runT)(_.left toStream)

    def toStream(implicit i: F <~> Id): Stream[A] =
      e.run.left.toStream

    def map[C](f: (=> A) => C)(implicit F: Functor[F]): LazyEitherT[C, F, B] =
      lazyEitherT(F.map(e.runT)(_.left map f))

    def foreach(f: (=> A) => Unit)(implicit F: Each[F]): Unit =
      F.each(e.runT)(_.left foreach f)

    def flatMap[C](f: (=> A) => LazyEitherT[C, F, B])(implicit M: Monad[F]): LazyEitherT[C, F, B] =
      LazyEitherT(M.bind(e.runT)(_.fold(a => f(a).runT, b => M.pure(LazyEither.lazyRight[C](b)))))
  }

}

trait LazyEitherTInstances {
  implicit def lazyEitherTBiFunctor[F[_] : Functor]: BiFunctor[({type λ[α, β] = LazyEitherT[α, F, β]})#λ] = new BiFunctor[({type λ[α, β] = LazyEitherT[α, F, β]})#λ] {
    def bimap[A, B, C, D](fab: LazyEitherT[A, F, B])(f: A => C, g: B => D) =
      fab.map(x => g(x)).left.map(x => f(x))
  }

  implicit def LazyEitherTMonadTrans[Z]: MonadTrans[({type λ[α[_], β] = LazyEitherT[Z, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = LazyEitherT[Z, α, β]})#λ] {
    def hoist[M[_], N[_]](f: M ~> N) = new (({type f[x] = LazyEitherT[Z, M, x]})#f ~> ({type f[x] = LazyEitherT[Z, N, x]})#f) {
      def apply[A](fa: LazyEitherT[Z, M, A]): LazyEitherT[Z, N, A] = LazyEitherT(f.apply(fa.runT))
    }

    def liftM[G[_] : Monad, A](a: G[A]): LazyEitherT[Z, G, A] = LazyEitherT(Monad[G].map(a)((a: A) => LazyEither.lazyRight(a): LazyEither[Z, A]))
  }
}

trait LazyEitherTFunctions {
  def lazyEitherT[A, F[_], B](a: F[LazyEither[A, B]]): LazyEitherT[A, F, B] = new LazyEitherT[A, F, B] {
    val runT = a
  }

  import LazyEither._

  def lazyLeftT[A, F[_], B](a: => A)(implicit p: Pointed[F]): LazyEitherT[A, F, B] =
    lazyEitherT(p.pure(lazyLeft(a)))

  def lazyRightT[A, F[_], B](b: => B)(implicit p: Pointed[F]): LazyEitherT[A, F, B] =
    lazyEitherT(p.pure(lazyRight(b)))
}
