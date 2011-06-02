package scalaz

sealed trait OptionT[F[_], A] {
  val runT: F[Option[A]]

  import OptionT._

  def *->* : (({type λ[α] = OptionT[F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = OptionT[F, α]})#λ, A](this)

  def run(implicit i: F[Option[A]] =:= Ident[Option[A]]): Option[A] =
    runT.value

  def isDefinedT(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: Option[A]).isDefined)(runT)

  def isDefined(implicit i: F[Option[A]] =:= Ident[Option[A]]): Boolean =
    run.isDefined

  def isEmptyT(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: Option[A]).isEmpty)(runT)

  def isEmpty(implicit i: F[Option[A]] =:= Ident[Option[A]]): Boolean =
    run.isEmpty

  def getOrElseT(default: => A)(implicit ftr: Functor[F]): F[A] =
    ftr.fmap((_: Option[A]).getOrElse(default))(runT)

  def getOrElse(default: => A)(implicit i: F[Option[A]] =:= Ident[Option[A]]): A =
    run.getOrElse(default)

  def existsT(f: A => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: Option[A]).exists(f))(runT)

  def exists(f: A => Boolean)(implicit i: F[Option[A]] =:= Ident[Option[A]]): Boolean =
    run.exists(f)

  def forallT(f: A => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    ftr.fmap((_: Option[A]).forall(f))(runT)

  def forall(f: A => Boolean)(implicit i: F[Option[A]] =:= Ident[Option[A]]): Boolean =
    run.forall(f)

  def orElseT(a: => Option[A])(implicit ftr: Functor[F]): OptionT[F, A] =
    optionT(ftr.fmap((_: Option[A]).orElse(a))(OptionT.this.runT))

  def orElse(a: => Option[A])(implicit i: F[Option[A]] =:= Ident[Option[A]]): Option[A] =
    run.orElse(a)

  def map[B](f: A => B)(implicit ftr: Functor[F]): OptionT[F, B] =
    optionT(ftr.fmap((_: Option[A]) map f)(runT))

  def foreach(f: A => Unit)(implicit e: Each[F]): Unit =
    e.each((_: Option[A]) foreach f)(runT)

  def filter(f: A => Boolean)(implicit ftr: Functor[F]): OptionT[F, A] =
    optionT(ftr.fmap((_: Option[A]).filter(f))(runT))

  def flatMap[B](f: A => OptionT[F, B])(implicit m: Monad[F]): OptionT[F, B] =
    optionT(m.bd((o: Option[A]) => o match {
      case None => m.point(None: Option[B])
      case Some(a) => f(a).runT
    })(runT))

  def mapOption[B](f: Option[A] => Option[B])(implicit ftr: Functor[F]): OptionT[F, B] =
    optionT(ftr.fmap(f)(runT))
}

object OptionT extends OptionTs {
  def apply[F[_], A](r: F[Option[A]]): OptionT[F, A] =
    optionT(r)
}

trait OptionTs {
  def optionT[F[_], A](r: F[Option[A]]): OptionT[F, A] = new OptionT[F, A] {
    val runT = r
  }

  def someT[F[_], A](implicit p: Pointed[F]): (=> A) => OptionT[F, A] =
    a => optionT(p.point(Some(a)))

  def noneT[F[_], A](implicit p: Pointed[F]): OptionT[F, A] =
    optionT(p.point(None))

  implicit def EqualOptionT[F[_], A](implicit e: Equal[F[Option[A]]]): Equal[OptionT[F, A]] =
    Equal.equalBy(_.runT)

  implicit def OrderOptionT[F[_], A](implicit e: Order[F[Option[A]]]): Order[OptionT[F, A]] =
    Order.orderBy(_.runT)

  implicit def ShowOptionT[F[_], A](implicit e: Show[F[Option[A]]]): Show[OptionT[F, A]] =
    Show.showBy(_.runT)

  implicit val OptionTMonadTrans: MonadTrans[OptionT] = new MonadTrans[OptionT] {
    def lift[G[_] : Monad, A](a: G[A]): OptionT[G, A] =
      optionT(implicitly[Monad[G]].fmap((a: A) => Some(a): Option[A])(a))
  }
}