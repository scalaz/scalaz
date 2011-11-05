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

//
// Prioritized Implicits for type class instances
//

trait LazyOptionTInstances2 {
  implicit def lazyOptionTFunctor[F[_]](implicit F0: Functor[F]): Functor[({type λ[α] = LazyOptionT[F, α]})#λ] = new LazyOptionTFunctor[F] {
    implicit def F: Functor[F] = F0
  }
}

trait LazyOptionTInstances1 extends LazyOptionTInstances2 {
  implicit def lazyOptionTPointed[F[_]](implicit F0: Pointed[F]): Pointed[({type λ[α] = LazyOptionT[F, α]})#λ] = new LazyOptionTPointed[F] {
    implicit def F: Pointed[F] = F0
  }
}

trait LazyOptionTInstances0 extends LazyOptionTInstances1 {
  implicit def lazyOptionTApply[F[_]](implicit F0: Apply[F]): Apply[({type λ[α] = LazyOptionT[F, α]})#λ] = new LazyOptionTApply[F] {
      implicit def F: Apply[F] = F0
    }
}

trait LazyOptionTInstances extends LazyOptionTInstances0 {
  implicit def lazyOptionTMonadTrans: MonadTrans[LazyOptionT] = new LazyOptionTMonadTrans {}

  implicit def lazyOptionTMonad[F[_]](implicit F0: Monad[F]): Monad[({type λ[α] = LazyOptionT[F, α]})#λ] = new LazyOptionTMonad[F] {
    implicit def F: Monad[F] = F0
  }
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


//
// Implementation traits for type class instances
//

private[scalaz] trait LazyOptionTFunctor[F[_]] extends Functor[({type λ[α] = LazyOptionT[F, α]})#λ] {
  implicit def F: Functor[F]

  def map[A, B](fa: LazyOptionT[F, A])(f: A => B): LazyOptionT[F, B] = fa map (a => f(a))
}

private[scalaz] trait LazyOptionTPointed[F[_]] extends LazyOptionTFunctor[F] with Pointed[({type λ[α] = LazyOptionT[F, α]})#λ] {
  implicit def F: Pointed[F]

  def point[A](a: => A): LazyOptionT[F, A] = LazyOptionT[F, A](F.point(LazyOption.lazySome(a)))
}

private[scalaz] trait LazyOptionTApply[F[_]] extends LazyOptionTFunctor[F] with Apply[({type λ[α] = LazyOptionT[F, α]})#λ] {
  implicit def F: Apply[F]

  def ap[A, B](fa: LazyOptionT[F, A])(f: LazyOptionT[F, A => B]): LazyOptionT[F, B] =
    LazyOptionT(F.map2(f.runT, fa.runT)({ case (ff, aa) => LazyOption.lazyOptionInstance.ap(aa)(ff) }))
}

private[scalaz] trait LazyOptionTMonad[F[_]] extends LazyOptionTPointed[F] with Monad[({type λ[α] = LazyOptionT[F, α]})#λ] {
  implicit def F: Monad[F]

  def bind[A, B](fa: LazyOptionT[F, A])(f: A => LazyOptionT[F, B]): LazyOptionT[F, B] = fa flatMap (a => f(a))
}

private[scalaz] trait LazyOptionTMonadTrans extends MonadTrans[LazyOptionT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): LazyOptionT[G, A] =
    LazyOptionT[G, A](G.map[A, LazyOption[A]](a)((a: A) => LazyOption.lazySome(a)))

  def hoist[M[_], N[_]](f: M ~> N) = new (({type f[x] = LazyOptionT[M, x]})#f ~> ({type f[x] = LazyOptionT[N, x]})#f) {
    def apply[A](fa: LazyOptionT[M, A]): LazyOptionT[N, A] = LazyOptionT(f.apply(fa.runT))
  }
}
