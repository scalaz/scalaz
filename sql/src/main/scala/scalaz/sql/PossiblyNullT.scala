package scalaz
package sql


// isomorphic to OptionT[F, A]
sealed trait PossiblyNullT[F[_], A] {
  val toOptionT: OptionT[F, A] = this.asInstanceOf[PossiblyNullT_[F, A]].o

  def *->* : (({type λ[α] = PossiblyNullT[F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = PossiblyNullT[F, α]})#λ, A](this)

  import =~~=._
  import OptionT._
  import PossiblyNullT._

  def toOption(implicit i: F =~~= Identity): Option[A] =
    toOptionT.runT

  def toMaybe(implicit i: F =~~= Identity): Maybe[A] =
    optionT[Identity, A](Identity.id(toOptionT.runT))

  def foldT[X](nn: A => X, in: => X)(implicit ftr: Functor[F]): F[X] =
    toOptionT.foldT(nn, in)

  def fold[X](nn: A => X, in: => X)(implicit i: F =~~= Identity): X =
    toOption match {
      case Some(a) => nn(a)
      case None => in
    }

  def map[B](f: A => B)(implicit ftr: Functor[F]): PossiblyNullT[F, B] =
    PossiblyNullT_(toOptionT map f)

  def flatMap[B](f: A => PossiblyNullT[F, B])(implicit m: Monad[F]): PossiblyNullT[F, B] =
    PossiblyNullT_(toOptionT flatMap (f(_).toOptionT))

  def filter(f: A => Boolean)(implicit ftr: Functor[F]): PossiblyNullT[F, A] =
    PossiblyNullT_(toOptionT filter f)

  def foreach(f: A => Unit)(implicit e: Each[F]): Unit =
    toOptionT foreach f

  def filterM(f: A => F[Boolean])(implicit m: Monad[F]): PossiblyNullT[F, A] =
    PossiblyNullT_(toOptionT filterM f)

  def mapPossiblyNull[B](f: PossiblyNull[A] => PossiblyNull[B])(implicit ftr: Functor[F]): PossiblyNullT[F, B] =
    PossiblyNullT_(optionT(foldT(
      a => f(notNull(a)).toOption
    , None
    )))

  def isNullT(implicit ftr: Functor[F]): F[Boolean] =
    implicitly[Functor[F]].fmap((o: Option[A]) => o.isEmpty)(toOptionT.runT)

  def isNull(implicit i: F =~~= Identity): Boolean =
    toOption.isEmpty

  def isNotNullT(implicit ftr: Functor[F]): F[Boolean] =
    implicitly[Functor[F]].fmap((o: Option[A]) => o.isDefined)(toOptionT.runT)

  def isNotNull(implicit i: F =~~= Identity): Boolean =
    toOption.isDefined

  def ifelseT[X](nn: => X, in: => X)(implicit ftr: Functor[F]): F[X] =
    implicitly[Functor[F]].fmap((o: Option[A]) => o match {
      case None => in
      case Some(_) => nn
    })(toOptionT.runT)

  def ifelse[X](nn: => X, in: => X)(implicit i: F =~~= Identity): X =
    toOption match {
      case None => in
      case Some(_) => nn
    }

  def toListT(implicit ftr: Functor[F]): F[List[A]] =
    implicitly[Functor[F]].fmap((o: Option[A]) => o.toList)(toOptionT.runT)

  def toList(implicit i: F =~~= Identity): List[A] =
    toOption.toList

  def toStreamT(implicit ftr: Functor[F]): F[Stream[A]] =
    implicitly[Functor[F]].fmap((o: Option[A]) => o.toStream)(toOptionT.runT)

  def toStream(implicit i: F =~~= Identity): Stream[A] =
    toOption.toStream

  def getOrElseT(default: => A)(implicit ftr: Functor[F]): F[A] =
    toOptionT getOrElseT default

  def getOrElse(default: => A)(implicit i: F =~~= Identity): A =
    toOption getOrElse default

  def existsT(f: A => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    toOptionT existsT f

  def exists(f: A => Boolean)(implicit i: F =~~= Identity): Boolean =
    toOption exists f

  def forallT(f: A => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    toOptionT forallT f

  def forall(f: A => Boolean)(implicit i: F =~~= Identity): Boolean =
    toOption forall f

  def orElseT(a: => PossiblyNull[A])(implicit ftr: Functor[F]): PossiblyNullT[F, A] =
    PossiblyNullT_(toOptionT orElseT a.toOption)

  def orElse(a: => PossiblyNull[A])(implicit i: F =~~= Identity): PossiblyNull[A] =
    PossiblyNullT_(toMaybe orElseT a.toOption)

  def toJdbcTypeT(k: A => F[JdbcType])(implicit m: Monad[F]): F[JdbcType] = {
    implicit val ftr = m.functor
    implicitly[Monad[F]].bd(k)(getOrElseT (null.asInstanceOf[A]))
  }

  def toJdbcType(k: A => JdbcType)(implicit i: F =~~= Identity): JdbcType =
    k(toOption getOrElse null.asInstanceOf[A])

}
private case class PossiblyNullT_[F[_], A](o: OptionT[F, A]) extends PossiblyNullT[F, A]

object PossiblyNullT extends PossiblyNullTs {
  def apply[F[_], A](a: F[A])(implicit ftr: Functor[F]): PossiblyNullT[F, A] =
    notNullT(a)
}

trait PossiblyNullTs {
  type PossiblyNull[A] =
  PossiblyNullT[Identity, A]

  def notNullT[F[_], A](a: F[A])(implicit ftr: Functor[F]): PossiblyNullT[F, A] =
    PossiblyNullT_(OptionT(ftr.fmap((aa: A) => Some(aa): Option[A])(a)))

  def isNullT[F[_], A](implicit p: Pointed[F]): PossiblyNullT[F, A] =
    PossiblyNullT_(OptionT(p.point(None: Option[A])))

  def notNull[A]: A => PossiblyNull[A] =
    a => notNullT(Identity.id(a))

  def isNull[A]: PossiblyNull[A] =
    isNullT[Identity, A]

  def fromOptionT[F[_], A](o: OptionT[F, A]): PossiblyNullT[F, A] =
    PossiblyNullT_(o)

  implicit def PossiblyNullTFunctor[F[_]: Functor]: Functor[({type λ[α] = PossiblyNullT[F, α]})#λ] = new Functor[({type λ[α] = PossiblyNullT[F, α]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def PossiblyNullTApplic[F[_]: ApplicFunctor]: Applic[({type λ[α] = PossiblyNullT[F, α]})#λ] = new Applic[({type λ[α] = PossiblyNullT[F, α]})#λ] {
    def applic[A, B](f: PossiblyNullT[F, A => B]) =
      a => fromOptionT(implicitly[Applic[({type λ[α] = OptionT[F, α]})#λ]].applic(f.toOptionT)(a.toOptionT))
  }

  implicit def PossiblyNullTBind[F[_]: Monad]: Bind[({type λ[α] = PossiblyNullT[F, α]})#λ] = new Bind[({type λ[α] = PossiblyNullT[F, α]})#λ] {
    def bind[A, B](f: A => PossiblyNullT[F, B]) =
      _ flatMap f
  }

  implicit def PossiblyNullTPointed[F[_]: Pointed]: Pointed[({type λ[α] = PossiblyNullT[F, α]})#λ] = new Pointed[({type λ[α] = PossiblyNullT[F, α]})#λ] {
    def point[A](a: => A) =
      fromOptionT(implicitly[Pointed[({type λ[α] = OptionT[F, α]})#λ]].point(a))
  }

  implicit def PossiblyNullTApplicFunctor[F[_]: ApplicFunctor]: ApplicFunctor[({type λ[α] = PossiblyNullT[F, α]})#λ] = {
    implicit val ftr = implicitly[ApplicFunctor[F]].functor
    ApplicFunctor.applicFunctor[({type λ[α] = PossiblyNullT[F, α]})#λ]
  }

  implicit def PossiblyNullTApplicative[F[_]: Applicative]: Applicative[({type λ[α] = PossiblyNullT[F, α]})#λ] = {
    implicit val ftr = implicitly[Applicative[F]].pointedFunctor
    implicit val ap = implicitly[Applicative[F]].applicFunctor
    Applicative.applicative[({type λ[α] = PossiblyNullT[F, α]})#λ]
  }

  implicit def PossiblyNullTBindFunctor[F[_]: Monad]: BindFunctor[({type λ[α] = PossiblyNullT[F, α]})#λ] = {
    implicit val ftr = implicitly[Monad[F]].functor
    BindFunctor.bindFunctor[({type λ[α] = PossiblyNullT[F, α]})#λ]
  }

  implicit def PossiblyNullTPointedFunctor[F[_]: PointedFunctor]: PointedFunctor[({type λ[α] = PossiblyNullT[F, α]})#λ] = {
    implicit val p = implicitly[PointedFunctor[F]].pointed
    implicit val ftr = implicitly[PointedFunctor[F]].functor
    PointedFunctor.pointedFunctor[({type λ[α] = PossiblyNullT[F, α]})#λ]
  }

  implicit def PossiblyNullTMonad[F[_]: Monad]: Monad[({type λ[α] = PossiblyNullT[F, α]})#λ] = {
    implicit val bd = implicitly[Monad[F]].bind
    implicit val p = implicitly[Monad[F]].pointed
    Monad.monadBP[({type λ[α] = PossiblyNullT[F, α]})#λ]
  }

  implicit def PossiblyNullTFoldr[F[_]: Foldr]: Foldr[({type λ[α] = PossiblyNullT[F, α]})#λ] = new Foldr[({type λ[α] = PossiblyNullT[F, α]})#λ] {
    def foldr[A, B] = k => b => s =>
      implicitly[Foldr[({type λ[α] = OptionT[F, α]})#λ]].foldr(k)(b)(s.toOptionT)
  }

  implicit def PossiblyNullTFoldl[F[_]: Foldl]: Foldl[({type λ[α] = PossiblyNullT[F, α]})#λ] = new Foldl[({type λ[α] = PossiblyNullT[F, α]})#λ] {
    def foldl[A, B] = k => b => s =>
      implicitly[Foldl[({type λ[α] = OptionT[F, α]})#λ]].foldl(k)(b)(s.toOptionT)
  }

  implicit def PossiblyNullTTraverse[F[_]: Traverse]: Traverse[({type λ[α] = PossiblyNullT[F, α]})#λ] =
    implicitly[Traverse[({type λ[α] = OptionT[F, α]})#λ]].xmap[({type λ[α] = PossiblyNullT[F, α]})#λ](
      new (({type λ[α] = OptionT[F, α]})#λ ~> ({type λ[α] = PossiblyNullT[F, α]})#λ) {
        def apply[A](a: OptionT[F, A]) = fromOptionT(a)
      }
    , new (({type λ[α] = PossiblyNullT[F, α]})#λ ~> ({type λ[α] = OptionT[F, α]})#λ) {
        def apply[A](a: PossiblyNullT[F, A]) = a.toOptionT
      }
    )

  implicit def PossiblyNullTPlus[F[_]](implicit m: ApplicFunctor[F]): Plus[({type λ[α] = PossiblyNullT[F, α]})#λ] = new Plus[({type λ[α] = PossiblyNullT[F, α]})#λ] {
    def plus[A](a1: PossiblyNullT[F, A], a2: => PossiblyNullT[F, A]) =
      PossiblyNullT_(OptionT(m.liftA2((a1: Option[A]) => (a2: Option[A]) => a1 orElse a2)(a1.toOptionT.runT)(a2.toOptionT.runT)))
  }

  implicit def PossiblyNullTEmpty[F[_]](implicit p: Pointed[F]): Empty[({type λ[α] = PossiblyNullT[F, α]})#λ] = new Empty[({type λ[α] = PossiblyNullT[F, α]})#λ] {
    def empty[A] =
      PossiblyNullT_(OptionT.noneT[F, A])
  }

  implicit def PossiblyNullTMonadEmpty[F[_]: Monad]: MonadEmpty[({type λ[α] = PossiblyNullT[F, α]})#λ] = {
    implicit val p = implicitly[Monad[F]].pointed
    MonadEmpty.monadEmpty[({type λ[α] = PossiblyNullT[F, α]})#λ]
  }

  implicit def PossiblyNullTMonadEmptyPlus[F[_]: Monad]: MonadEmptyPlus[({type λ[α] = PossiblyNullT[F, α]})#λ] = {
    implicit val p = implicitly[Monad[F]].pointed
    implicit val ap = implicitly[Monad[F]].applicFunctor
    MonadEmptyPlus.monadEmptyPlus[({type λ[α] = PossiblyNullT[F, α]})#λ]
  }

  implicit def PossiblyNullZero[A]: Zero[PossiblyNull[A]] =
    Zero.zero(isNull)

  implicit def PossiblyNullSemigroup[A]: Semigroup[PossiblyNull[A]] =
    Semigroup.semigroup(a1 => a2 => a1 fold (_ => a1, a2 fold (_ => a2, a1)))

  implicit def PossiblyNullShow[A](implicit s: Show[A]): Show[PossiblyNull[A]] =
    Show.shows(_.fold(
      a => ("not-null(" + s.shows(a) + ")")
    , "is-null"
    ))

  implicit def PossiblyNullEqual[A: Equal]: Equal[PossiblyNull[A]] =
    implicitly[Equal[Option[A]]].contramap(_.toOption)

  implicit def PossiblyNullOrder[A: Order]: Order[PossiblyNull[A]] =
    implicitly[Order[Option[A]]].contramap(_.toOption)

  implicit val PossiblyNullTMonadTrans: MonadTrans[PossiblyNullT] = new MonadTrans[PossiblyNullT] {
    def lift[G[_] : Monad, A](a: G[A]): PossiblyNullT[G, A] =
      PossiblyNullT_(implicitly[MonadTrans[OptionT]].lift(a))
  }

}
