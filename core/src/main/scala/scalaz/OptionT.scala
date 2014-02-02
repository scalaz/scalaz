package scalaz

import std.option._

/**
 * OptionT monad transformer.
 */
final case class OptionT[F[_], A](run: F[Option[A]]) extends TraverseTImpl[OptionT, Option, F, A] {
  def T = OptionT.T

  @deprecated("Each/foreach is deprecated", "7.1")
  def foreach(f: A => Unit)(implicit E: Each[F]): Unit = E.each(run)(_ foreach f)

  /** Apply a function in the environment of both options, containing
    * both `F`s.  It is not compatible with `Monad#bind`.
    */
  def app[B](f: => OptionT[F, A => B])(implicit F: Apply[F]): OptionT[F, B] =
    OptionT(F.apply2(f.run, run) {
      case (ff, aa) => optionInstance.ap(aa)(ff)
    })

  def isDefined(implicit F: Functor[F]): F[Boolean] = mapO(_.isDefined)

  def isEmpty(implicit F: Functor[F]): F[Boolean] = mapO(_.isEmpty)

  def fold[X](some: A => X, none: => X)(implicit F: Functor[F]): F[X] =
    mapO {
      case None => none
      case Some(a) => some(a)
    }

  def getOrElse(default: => A)(implicit F: Functor[F]): F[A] = mapO(_.getOrElse(default))

  def orElse(a: => OptionT[F, A])(implicit F: Monad[F]): OptionT[F, A] =
    OptionT(F.bind(run) {
      case None => a.run
      case x@Some(_) => F.point(x)
    })

  /** @since 7.0.3 */
  def toRight[E](e: => E)(implicit F: Functor[F]): EitherT[F,E,A] = EitherT(F.map(run)(std.option.toRight(_)(e)))

  /** @since 7.0.3 */
  def toLeft[B](b: => B)(implicit F: Functor[F]): EitherT[F,A,B] = EitherT(F.map(run)(std.option.toLeft(_)(b)))

  private def mapO[B](f: Option[A] => B)(implicit F: Functor[F]) = F.map(run)(f)
}

object OptionT extends OptionTInstances with OptionTFunctions with TraverseTImpl.`(*->*)->*->*`[OptionT, Option] {
  def T = new TraverseTWrapper[OptionT, Option] {
    def wrap[M[_], A](run: M[Option[A]]): OptionT[M, A] = OptionT(run)
    def unwrap[M[_], A](t: OptionT[M, A]): M[Option[A]] = t.run
    def TraverseN = Traverse[Option]
  }
}

sealed abstract class OptionTInstances {
  implicit def optionTEqual[F[_], A](implicit F0: Equal[F[Option[A]]]): Equal[OptionT[F, A]] = F0.contramap((_: OptionT[F, A]).run)
}

trait OptionTFunctions {
  def monadTell[F[_, _], W, A](implicit MT0: MonadTell[F, W]): MonadTell[({type λ[α, β]=OptionT[({type f[x]=F[α, x]})#f, β] })#λ, W] = new OptionTMonadTell[F, W] {
    def MT = MT0
    def T = OptionT.T
  }

  def monadListen[F[_, _], W, A](implicit ML0: MonadListen[F, W]): MonadListen[({type λ[α, β]=OptionT[({type f[x]=F[α, x]})#f, β] })#λ, W] = new OptionTMonadListen[F, W] {
    def MT = ML0
    def T = OptionT.T
  }
}

//
// Implementation traits for type class instances
//

private trait OptionTMonadTell[F[_, _], W]
  extends MonadTell[({ type λ[α, β] = OptionT[({ type f[x] = F[α, x] })#f, β] })#λ, W]
  with TraverseTMonad[OptionT, Option, ({ type λ[α] = F[W, α] })#λ]
  with TraverseTHoist[OptionT, Option] {

  def MT: MonadTell[F, W]

  implicit def M = MT
  def N = Monad[Option]

  def writer[A](w: W, v: A): OptionT[({ type λ[α] = F[W, α] })#λ, A] =
    liftM[({ type λ[α] = F[W, α] })#λ, A](MT.writer(w, v))

  def some[A](v: => A): OptionT[({ type λ[α] = F[W, α] })#λ, A] =
    OptionT.traverseT[({ type λ[α] = F[W, α] })#λ].apply[A](MT.point(Some(v)))

  def none[A]: OptionT[({ type λ[α] = F[W, α] })#λ, A] =
    OptionT.traverseT[({ type λ[α] = F[W, α] })#λ].apply[A](MT.point(None))
}

private trait OptionTMonadListen[F[_, _], W] extends MonadListen[({ type λ[α, β] = OptionT[({ type f[x] = F[α, x] })#f, β] })#λ, W] with OptionTMonadTell[F, W] {
  def MT: MonadListen[F, W]

  def listen[A](ma: OptionT[({ type λ[α] = F[W, α] })#λ, A]): OptionT[({ type λ[α] = F[W, α] })#λ, (A, W)] = {
    val tmp = MT.bind[(Option[A], W), Option[(A, W)]](MT.listen(ma.run)) {
      case (None, _) => MT.point(None)
      case (Some(a), w) => MT.point(Some(a, w))
    }

    OptionT.traverseT[({ type λ[α] = F[W, α] })#λ].apply[(A, W)](tmp)
  }
}
