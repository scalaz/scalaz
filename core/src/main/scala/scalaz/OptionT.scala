package scalaz

import std.option.{optionInstance, none, some}

/**
 * OptionT monad transformer.
 */
final case class OptionT[F[_], A](run: F[Option[A]]) {
  self =>

  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] = new OptionT[F, B](mapO(_ map f))

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] = new OptionT[F, B](
    F.bind(self.run) {
      case None    => F.point(None: Option[B])
      case Some(z) => f(z).run
    }
  )

  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]): OptionT[F, B] = new OptionT[F, B](
    F.bind(self.run) {
      case None    => F.point(none[B])
      case Some(z) => F.map(f(z))(b => some(b))
    }
  )

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F]): Z = {
    import std.option._
    F.foldRight[Option[A], Z](run, z)((a, b) => Foldable[Option].foldRight[A, Z](a, b)(f))
  }

  def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[OptionT[F, B]] = {
    import std.option._
    G.map(F.traverse(run)(o => Traverse[Option].traverse(o)(f)))(OptionT(_))
  }

  def ap[B](f: => OptionT[F, A => B])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.bind(f.run){
              case None => F.point(None)
              case Some(ff) => F.map(run)(_ map ff)
            })

  /** Apply a function in the environment of both options, containing
    * both `F`s.  It is not compatible with `Monad#bind`.
    */
  def app[B](f: => OptionT[F, A => B])(implicit F: Apply[F]): OptionT[F, B] =
    OptionT(F.apply2(f.run, run) {
      case (ff, aa) => optionInstance.ap(aa)(ff)
    })

  def isDefined(implicit F: Functor[F]): F[Boolean] = mapO(_.isDefined)

  def isEmpty(implicit F: Functor[F]): F[Boolean] = mapO(_.isEmpty)

  def filter(f: A => Boolean)(implicit F: Functor[F]): OptionT[F, A] = OptionT(F.map(self.run) { _ filter f })

  def fold[X](some: A => X, none: => X)(implicit F: Functor[F]): F[X] =
    mapO {
      case None => none
      case Some(a) => some(a)
    }

  def getOrElse(default: => A)(implicit F: Functor[F]): F[A] = mapO(_.getOrElse(default))

  /** Alias for `getOrElse`. */
  def |(default: => A)(implicit F: Functor[F]): F[A] = getOrElse(default)

  def getOrElseF(default: => F[A])(implicit F: Monad[F]): F[A] =
    F.bind(self.run) {
      case None => default
      case Some(a) => F.point(a)
    }

  def orZero(implicit F0: Functor[F], M0: Monoid[A]): F[A] = getOrElse(M0.zero)

  def unary_~(implicit F0: Functor[F], M0: Monoid[A]): F[A] = orZero

  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] = mapO(_.exists(f))

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] = mapO(_.forall(f))

  def orElse(a: => OptionT[F, A])(implicit F: Monad[F]): OptionT[F, A] =
    OptionT(F.bind(run) {
      case None => a.run
      case x@Some(_) => F.point(x)
    })

  def |||(a: => OptionT[F, A])(implicit F: Monad[F]): OptionT[F, A] =
    orElse(a)

  /** @since 7.0.3 */
  def toRight[E](e: => E)(implicit F: Functor[F]): EitherT[F,E,A] = EitherT(F.map(run)(std.option.toRight(_)(e)))

  def toListT(implicit F: Functor[F]) : ListT[F, A] =  ListT[F,A](F.map(run)(_.toList))

  /** @since 7.0.3 */
  def toLeft[B](b: => B)(implicit F: Functor[F]): EitherT[F,A,B] = EitherT(F.map(run)(std.option.toLeft(_)(b)))

  private def mapO[B](f: Option[A] => B)(implicit F: Functor[F]) = F.map(run)(f)
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class OptionTInstances3 {
  implicit def optionTFunctor[F[_]](implicit F0: Functor[F]): Functor[OptionT[F, ?]] =
    new OptionTFunctor[F] {
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class OptionTInstances2 extends OptionTInstances3 {
  implicit def optionTBindRec[F[_]](implicit F0: Monad[F], B0: BindRec[F]): BindRec[OptionT[F, ?]] =
    new OptionTBindRec[F] {
      implicit def F = F0
      implicit def B = B0
    }
}

sealed abstract class OptionTInstances1 extends OptionTInstances2 {
  implicit def optionTFoldable[F[_]](implicit F0: Foldable[F]): Foldable[OptionT[F, ?]] =
    new OptionTFoldable[F] {
      implicit def F: Foldable[F] = F0
    }

  implicit def optionTMonadError[F[_], E](implicit F0: MonadError[F, E]): MonadError[OptionT[F, ?], E] =
    new OptionTMonadError[F, E] {
      def F = F0
    }
}

sealed abstract class OptionTInstances0 extends OptionTInstances1 {
  implicit def optionTMonadPlus[F[_]](implicit F0: Monad[F]): MonadPlus[OptionT[F, ?]] =
    new OptionTMonadPlus[F] {
      implicit def F: Monad[F] = F0
    }
}

sealed abstract class OptionTInstances extends OptionTInstances0 {
  implicit val optionTMonadTrans: Hoist[OptionT] = new OptionTHoist {}

  implicit def optionTTraverse[F[_]](implicit F0: Traverse[F]): Traverse[OptionT[F, ?]] =
    new OptionTTraverse[F] {
      implicit def F: Traverse[F] = F0
    }

  implicit def optionTEqual[F[_], A](implicit F0: Equal[F[Option[A]]]): Equal[OptionT[F, A]] =
    F0.contramap((_: OptionT[F, A]).run)

  implicit def optionTShow[F[_], A](implicit F0: Show[F[Option[A]]]): Show[OptionT[F, A]] =
    Contravariant[Show].contramap(F0)(_.run)
}

object OptionT extends OptionTInstances {
  def optionT[M[_]] =
    new (λ[α => M[Option[α]]] ~> OptionT[M, ?]) {
      def apply[A](a: M[Option[A]]) = new OptionT[M, A](a)
    }

  def some[M[_], A](v: => A)(implicit M: Applicative[M]): OptionT[M, A] =
    OptionT.optionT[M].apply[A](M.point(Some(v)))

  def none[M[_], A](implicit M: Applicative[M]): OptionT[M, A] =
    OptionT.optionT[M].apply[A](M.point(None))

  def monadTell[F[_], W, A](implicit MT0: MonadTell[F, W]): MonadTell[OptionT[F, ?], W] =
    new OptionTMonadTell[F, W] {
      def MT = MT0
    }

  def monadListen[F[_], W, A](implicit ML0: MonadListen[F, W]): MonadListen[OptionT[F, ?], W] =
    new OptionTMonadListen[F, W] {
      def MT = ML0
    }
}

//
// Implementation traits for type class instances
//

private trait OptionTFunctor[F[_]] extends Functor[OptionT[F, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] = fa map f
}

private trait OptionTApply[F[_]] extends Apply[OptionT[F, ?]] with OptionTFunctor[F]{
  implicit def F: Monad[F]

  override final def ap[A, B](fa: => OptionT[F, A])(f: => OptionT[F, A => B]): OptionT[F, B] = fa ap f
}

private trait OptionTBind[F[_]] extends Bind[OptionT[F, ?]] with OptionTFunctor[F]{
  implicit def F: Monad[F]

  final def bind[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = fa flatMap f
}

private trait OptionTBindRec[F[_]] extends BindRec[OptionT[F, ?]] with OptionTBind[F] {
  implicit def F: Monad[F]
  implicit def B: BindRec[F]

  final def tailrecM[A, B](f: A => OptionT[F, A \/ B])(a: A): OptionT[F, B] =
    OptionT(
      B.tailrecM[A, Option[B]](a0 => F.map(f(a0).run) {
        _.fold(\/.right[A, Option[B]](None: Option[B]))(_.map(Some.apply))
      })(a)
    )
}

private trait OptionTMonad[F[_]] extends Monad[OptionT[F, ?]] with OptionTBind[F] {
  implicit def F: Monad[F]

  def point[A](a: => A): OptionT[F, A] = OptionT[F, A](F.point(some(a)))
}

private trait OptionTMonadError[F[_], E] extends MonadError[OptionT[F, ?], E] with OptionTMonad[F] {
  override def F: MonadError[F, E]

  override def raiseError[A](e: E) =
    OptionT[F, A](F.map(F.raiseError[A](e))(Some(_)))

  override def handleError[A](fa: OptionT[F, A])(f: E => OptionT[F, A]) =
    OptionT[F, A](F.handleError(fa.run)(f(_).run))
}

private trait OptionTFoldable[F[_]] extends Foldable.FromFoldr[OptionT[F, ?]] {
  implicit def F: Foldable[F]

  override def foldRight[A, B](fa: OptionT[F, A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
}

private trait OptionTTraverse[F[_]] extends Traverse[OptionT[F, ?]] with OptionTFoldable[F] with OptionTFunctor[F]{
  implicit def F: Traverse[F]

  def traverseImpl[G[_] : Applicative, A, B](fa: OptionT[F, A])(f: A => G[B]): G[OptionT[F, B]] = fa traverse f
}

private trait OptionTHoist extends Hoist[OptionT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): OptionT[G, A] =
    OptionT[G, A](G.map[A, Option[A]](a)((a: A) => some(a)))

  def hoist[M[_]: Monad, N[_]](f: M ~> N) =
    new (OptionT[M, ?] ~> OptionT[N, ?]) {
      def apply[A](fa: OptionT[M, A]): OptionT[N, A] = OptionT(f.apply(fa.run))
    }

  implicit def apply[G[_] : Monad]: Monad[OptionT[G, ?]] = OptionT.optionTMonadPlus[G]
}

private trait OptionTMonadPlus[F[_]] extends MonadPlus[OptionT[F, ?]] with OptionTMonad[F] {
  implicit def F: Monad[F]

  def empty[A]: OptionT[F, A] = OptionT(F point none[A])
  def plus[A](a: OptionT[F, A], b: => OptionT[F, A]): OptionT[F, A] = a orElse b
}

private trait OptionTMonadTell[F[_], W] extends MonadTell[OptionT[F, ?], W] with OptionTMonad[F] with OptionTHoist {
  def MT: MonadTell[F, W]

  implicit def F = MT

  def writer[A](w: W, v: A): OptionT[F, A] =
    liftM[F, A](MT.writer(w, v))
}

private trait OptionTMonadListen[F[_], W] extends MonadListen[OptionT[F, ?], W] with OptionTMonadTell[F, W] {
  def MT: MonadListen[F, W]

  def listen[A](ma: OptionT[F, A]): OptionT[F, (A, W)] = {
    val tmp = MT.bind[(Option[A], W), Option[(A, W)]](MT.listen(ma.run)) {
      case (None, _) => MT.point(None)
      case (Some(a), w) => MT.point(Some(a, w))
    }

    OptionT.optionT[F].apply[(A, W)](tmp)
  }
}
