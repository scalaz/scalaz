package scalaz
package sql


// isomorphic to OptionT[F, A]
sealed trait PossiblyNullT[F[_], A] {
  val toOptionT: OptionT[F, A] = this.asInstanceOf[PossiblyNullT_[F, A]].o

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
}
