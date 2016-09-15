package scalaz

final case class LazyOptionT[F[_], A](run: F[LazyOption[A]]) {
  import LazyOption._
  import LazyOptionT._
  import LazyEitherT._
  import EitherT._

  def ?[X](some: => X, none: => X)(implicit F: Functor[F]): F[X] =
    F.map(run)(_.?(some, none))

  def isDefined(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isDefined)

  def isEmpty(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isEmpty)

  def getOrElse(default: => A)(implicit F: Functor[F]): F[A] =
    F.map(run)(_.getOrElse(default))

  def |(default: => A)(implicit F: Functor[F]): F[A] =
    getOrElse(default)

  def orZero(implicit F0: Functor[F], M0: Monoid[A]): F[A] = getOrElse(M0.zero)

  def unary_~(implicit F0: Functor[F], M0: Monoid[A]): F[A] = orZero

  def exists(f: (=> A) => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.exists(f))

  def forall(f: (=> A) => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.forall(f))

  def toOption(implicit F: Functor[F]): OptionT[F, A] =
    OptionT.optionT(F.map(run)(_.toOption))

  def toLazyRight[X](left: => X)(implicit F: Functor[F]): LazyEitherT[F, X, A] =
    lazyEitherT(F.map(run)(_.toLazyRight(left)))

  def toLazyLeft[X](right: => X)(implicit F: Functor[F]): LazyEitherT[F, A, X] =
    lazyEitherT(F.map(run)(_.toLazyLeft(right)))

  def toRight[X](left: => X)(implicit F: Functor[F]): EitherT[F, X, A] =
    eitherT(F.map(run)(_.fold[X \/ A](z => \/-(z), -\/(left))))

  def toLeft[X](right: => X)(implicit F: Functor[F]): EitherT[F, A, X] =
    eitherT(F.map(run)(_.fold[A \/ X](z => -\/(z), \/-(right))))

  def orElse(a: => LazyOptionT[F, A])(implicit F: Monad[F]): LazyOptionT[F, A] =
    LazyOptionT(F.bind(run) {
      case LazyNone => a.run
      case x@LazySome(_) => F.point(x)
    })

  def map[B](f: (=> A) => B)(implicit F: Functor[F]): LazyOptionT[F, B] =
    lazyOptionT(F.map(run)(_ map f))

  def filter(f: (=> A) => Boolean)(implicit F: Functor[F]): LazyOptionT[F, A] =
    lazyOptionT(F.map(run)(_.filter(f)))

  def flatMap[B](f: (=> A) => LazyOptionT[F, B])(implicit M: Monad[F]): LazyOptionT[F, B] =
    lazyOptionT(M.bind(run)(_.fold(a => f(a).run, M.point(lazyNone[B]))))

  def mapLazyOption[B](f: LazyOption[A] => LazyOption[B])(implicit F: Functor[F]): LazyOptionT[F, B] =
    lazyOptionT(F.map(run)(f))

}

//
// Prioritized Implicits for type class instances
//

sealed abstract class LazyOptionTInstances1 {
  implicit def lazyOptionTFunctor[F[_]](implicit F0: Functor[F]): Functor[LazyOptionT[F, ?]] =
    new LazyOptionTFunctor[F] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class LazyOptionTInstances0 extends LazyOptionTInstances1 {
  implicit def lazyOptionEqual[F[_], A](implicit FA: Equal[F[LazyOption[A]]]): Equal[LazyOptionT[F, A]] =
    Equal.equalBy((_: LazyOptionT[F, A]).run)

  implicit def lazyOptionTMonadPlus[F[_]](implicit F0: Monad[F]): MonadPlus[LazyOptionT[F, ?]] =
    new LazyOptionTMonad[F] {
      implicit def F: Monad[F] = F0
    }
}

sealed abstract class LazyOptionTInstances extends LazyOptionTInstances0 {
  implicit val lazyOptionTMonadTrans: Hoist[LazyOptionT] =
    new LazyOptionTHoist {}

  implicit def lazyOptionTBindRec[F[_]](implicit F0: Monad[F], B0: BindRec[F]): BindRec[LazyOptionT[F, ?]] =
    new LazyOptionTBindRec[F] {
      implicit def F: Monad[F] = F0
      implicit def B: BindRec[F] = B0
    }

  implicit def lazyOptionOrder[F[_], A](implicit FA: Order[F[LazyOption[A]]]): Order[LazyOptionT[F, A]] =
    Order.orderBy((_: LazyOptionT[F, A]).run)
}

object LazyOptionT extends LazyOptionTInstances {
  def lazyOptionT[F[_], A](r: F[LazyOption[A]]): LazyOptionT[F, A] =
    LazyOptionT(r)

  import LazyOption._

  def lazySomeT[F[_], A](a: => A)(implicit F: Applicative[F]): LazyOptionT[F, A] =
    lazyOptionT(F.point(lazySome(a)))

  def lazyNoneT[F[_], A](implicit F: Applicative[F]): LazyOptionT[F, A] =
    lazyOptionT(F.point(lazyNone[A]))
}


//
// Implementation traits for type class instances
//

private trait LazyOptionTFunctor[F[_]] extends Functor[LazyOptionT[F, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: LazyOptionT[F, A])(f: A => B): LazyOptionT[F, B] =
    fa map (a => f(a))
}

private trait LazyOptionTMonad[F[_]] extends MonadPlus[LazyOptionT[F, ?]] with LazyOptionTFunctor[F] {
  implicit def F: Monad[F]

  override def ap[A, B](fa: => LazyOptionT[F, A])(f: => LazyOptionT[F, A => B]): LazyOptionT[F, B] =
    LazyOptionT(F.bind(f.run)(_ fold (ff => F.map(fa.run)(_ map ((ff:A=>B)(_))),
                                      F.point(LazyOption.lazyNone))))

  def point[A](a: => A): LazyOptionT[F, A] =
    LazyOptionT[F, A](F.point(LazyOption.lazySome(a)))

  def bind[A, B](fa: LazyOptionT[F, A])(f: A => LazyOptionT[F, B]): LazyOptionT[F, B] =
    fa flatMap (a => f(a))

  override def plus[A](a: LazyOptionT[F, A], b: => LazyOptionT[F, A]) =
    a orElse b

  override def empty[A] =
    LazyOptionT.lazyNoneT[F, A]
}

private trait LazyOptionTBindRec[F[_]] extends BindRec[LazyOptionT[F, ?]] with LazyOptionTMonad[F] {
  implicit def B: BindRec[F]

  final def tailrecM[A, B](f: A => LazyOptionT[F, A \/ B])(a: A): LazyOptionT[F, B] =
    LazyOptionT(
      B.tailrecM[A, LazyOption[B]](a => F.map(f(a).run) {
        _.fold(_.map(b => LazyOption.lazySome(b)), \/-(LazyOption.lazyNone))
      })(a)
    )
}

private trait LazyOptionTHoist extends Hoist[LazyOptionT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): LazyOptionT[G, A] =
    LazyOptionT[G, A](G.map[A, LazyOption[A]](a)((a: A) => LazyOption.lazySome(a)))

  def hoist[M[_]: Monad, N[_]](f: M ~> N) =
    λ[LazyOptionT[M, ?] ~> LazyOptionT[N, ?]](
      fa => LazyOptionT(f.apply(fa.run))
    )

  implicit def apply[G[_] : Monad]: Monad[LazyOptionT[G, ?]] =
    LazyOptionT.lazyOptionTMonadPlus[G]
}
