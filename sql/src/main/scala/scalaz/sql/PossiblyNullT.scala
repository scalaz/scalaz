package scalaz
package sql


// isomorphic to OptionT[F, A]
sealed trait PossiblyNullT[F[_], A] {
  val toOptionT: OptionT[F, A] = this.asInstanceOf[PossiblyNullT_[F, A]].o

  def map[B](f: A => B)(implicit ftr: Functor[F]): PossiblyNullT[F, B] =
    PossiblyNullT_(toOptionT map f)

  def flatMap[B](f: A => PossiblyNullT[F, B])(implicit m: Monad[F]): PossiblyNullT[F, B] =
    PossiblyNullT_(toOptionT flatMap (f(_).toOptionT))
}
private case class PossiblyNullT_[F[_], A](o: OptionT[F, A]) extends PossiblyNullT[F, A]

object PossiblyNullT extends PossiblyNullTs {
  def apply[F[_], A](a: F[A])(implicit ftr: Functor[F]): PossiblyNullT[F, A] =
    isNullT(a)
}

trait PossiblyNullTs {
  type PossiblyNull[A] =
  PossiblyNullT[Identity, A]

  def isNullT[F[_], A](a: F[A])(implicit ftr: Functor[F]): PossiblyNullT[F, A] =
    PossiblyNullT_(OptionT(ftr.fmap((aa: A) => Some(aa): Option[A])(a)))

  def notNullT[F[_], A](implicit p: Pointed[F]): PossiblyNullT[F, A] =
    PossiblyNullT_(OptionT(p.point(None: Option[A])))

  def isNull[A]: A => PossiblyNull[A] =
    a => isNullT(Identity.id(a))

  def notNull[A]: PossiblyNull[A] =
    notNullT[Identity, A]
}
