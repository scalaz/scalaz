package scalaz
package sql

import SqlExceptionContext._

sealed trait SqlValueT[F[_], A] {
  import SqlValueT._

  val value: EitherT[Err, F, A]

  def toEither(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Either[Err, A] =
    value.runT.value

  def ?[X](left: => X, right: => X)(implicit ftr: Functor[F]): F[X] =
    value ? (left, right)

  def -?-[X](left: => X, right: => X)(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): X =
    value -?- (left, right)

  def isErrorT(implicit ftr: Functor[F]): F[Boolean] =
    value isLeftT

  def isError(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Boolean =
    value isLeft

  def isValueT(implicit ftr: Functor[F]): F[Boolean] =
    value isRightT

  def isValue(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Boolean =
    value isRight

  def fold[X](e: Err => X, a: A => X)(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): X =
    value.run fold (e, a)

  def map[B](f: A => B)(implicit ftr: Functor[F]): SqlValueT[F, B] =
    eitherSqlValueT(value map f)

  def foreach[B](f: A => Unit)(implicit e: Each[F]): Unit =
    value foreach f

  def flatMap[B](f: A => SqlValueT[F, B])(implicit m: Monad[F]): SqlValueT[F, B] =
    eitherSqlValueT(value flatMap (f(_).value))

  def err: ErrProjectionT[F, A] = new ErrProjectionT[F, A] {
    val x = SqlValueT.this
  }

  def orT(default: => A)(implicit ftr: Functor[F]): F[A] =
    value getOrElseT default

  def or(default: => A)(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): A =
    value getOrElse default

  def existsT(f: A => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    value existsT f

  def exists(f: A => Boolean)(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Boolean =
    value exists f

  def forallT(f: A => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
    value forallT f

  def forall(f: A => Boolean)(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Boolean =
    value forall f

  def orElse(x: => SqlValueT[F, A])(implicit m: Bind[F]): SqlValueT[F, A] =
    eitherSqlValueT(value orElse x.value)

  def toOptionT(implicit ftr: Functor[F]): OptionT[F, A] =
    value toOptionT

  def toOption(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Option[A] =
    value toOption

  def toListT(implicit ftr: Functor[F]): F[List[A]] =
    value toListT

  def toList(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): List[A] =
    value toList

  def toStreamT(implicit ftr: Functor[F]): F[Stream[A]] =
    value toStreamT

  def toStream(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Stream[A] =
    value toStream

}

object SqlValueT extends SqlValueTs {
  def apply[F[_], A](a: EitherT[Err, F, A]): SqlValueT[F, A] =
    eitherSqlValueT(a)
}

trait SqlValueTs {
  type SqlException =
  java.sql.SQLException

  type SqlValue[A] =
  SqlValueT[Identity, A]

  type Err =
    SqlExceptionContext

  def eitherSqlValueT[F[_], A](a: EitherT[Err, F, A]): SqlValueT[F, A] = new SqlValueT[F, A] {
    val value = a
  }

  def eitherSqlValue[A](a: Either[Err, A]): SqlValue[A] =
    eitherSqlValueT(a.fold(
      EitherT.leftT(_)
    , EitherT.rightT(_)
    ))

  def sqlValueT[F[_], A](a: F[A])(implicit ftr: Functor[F]): SqlValueT[F, A] = new SqlValueT[F, A] {
    val value = EitherT.eitherT(ftr.fmap((a: A) => Right(a): Either[Err, A])(a))
  }

  def sqlErrorT[F[_], A](a: F[Err])(implicit ftr: Functor[F]): SqlValueT[F, A] = new SqlValueT[F, A] {
    val value = EitherT.eitherT(ftr.fmap((e: Err) => Left(e): Either[Err, A])(a))
  }

  def sqlErrorMessageT[F[_], A](a: F[String])(implicit ftr: Functor[F]): SqlValueT[F, A] =
    sqlErrorT(implicitly[Functor[F]].fmap((s: String) => sqlExceptionContext(new SqlException(s)))(a))

  def sqlValue[A]: A => SqlValue[A] =
    a => sqlValueT(Identity.id(a))

  def sqlError[A]: Err => SqlValue[A] =
    e => sqlErrorT(Identity.id(e))

  def sqlErrorMessage[A]: String => SqlValue[A] =
    s => sqlErrorMessageT(Identity.id(s))

  implicit def SqlValueTFunctor[F[_]: Functor]: Functor[({type λ[α] = SqlValueT[F, α]})#λ] = new Functor[({type λ[α] = SqlValueT[F, α]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def SqlValueTApplic[F[_]: ApplicFunctor]: Applic[({type λ[α] = SqlValueT[F, α]})#λ] = new Applic[({type λ[α] = SqlValueT[F, α]})#λ] {
    def applic[A, B](f: SqlValueT[F, A => B]) =
      a => eitherSqlValueT(implicitly[Applic[({type λ[α] = EitherT[Err, F, α]})#λ]].applic(f.value)(a.value))
  }

  implicit def SqlValueTBind[F[_]: Monad]: Bind[({type λ[α] = SqlValueT[F, α]})#λ] = new Bind[({type λ[α] = SqlValueT[F, α]})#λ] {
    def bind[A, B](f: A => SqlValueT[F, B]) =
      _ flatMap f
  }

  implicit def SqlValueTPointed[F[_]: Pointed]: Pointed[({type λ[α] = SqlValueT[F, α]})#λ] = new Pointed[({type λ[α] = SqlValueT[F, α]})#λ] {
    def point[A](a: => A) =
      eitherSqlValueT(implicitly[Pointed[({type λ[α] = EitherT[Err, F, α]})#λ]].point(a))
  }

  implicit def SqlValueTApplicFunctor[F[_]: ApplicFunctor]: ApplicFunctor[({type λ[α] = SqlValueT[F, α]})#λ] = {
    implicit val ftr = implicitly[ApplicFunctor[F]].functor
    ApplicFunctor.applicFunctor[({type λ[α] = SqlValueT[F, α]})#λ]
  }

  implicit def SqlValueTApplicative[F[_]: Applicative]: Applicative[({type λ[α] = SqlValueT[F, α]})#λ] = {
    implicit val ftr = implicitly[Applicative[F]].pointedFunctor
    implicit val ap = implicitly[Applicative[F]].applicFunctor
    Applicative.applicative[({type λ[α] = SqlValueT[F, α]})#λ]
  }

  implicit def SqlValueTBindFunctor[F[_]: Monad]: BindFunctor[({type λ[α] = SqlValueT[F, α]})#λ] = {
    implicit val ftr = implicitly[Monad[F]].functor
    BindFunctor.bindFunctor[({type λ[α] = SqlValueT[F, α]})#λ]
  }

  implicit def SqlValueTPointedFunctor[F[_]: PointedFunctor]: PointedFunctor[({type λ[α] = SqlValueT[F, α]})#λ] = {
    implicit val p = implicitly[PointedFunctor[F]].pointed
    implicit val ftr = implicitly[PointedFunctor[F]].functor
    PointedFunctor.pointedFunctor[({type λ[α] = SqlValueT[F, α]})#λ]
  }

  implicit def SqlValueTMonad[F[_]: Monad]: Monad[({type λ[α] = SqlValueT[F, α]})#λ] = {
    implicit val bd = implicitly[Monad[F]].bind
    implicit val p = implicitly[Monad[F]].pointed
    Monad.monadBP[({type λ[α] = SqlValueT[F, α]})#λ]
  }

  implicit def SqlValueTFoldr[F[_]: Foldr]: Foldr[({type λ[α] = SqlValueT[F, α]})#λ] = new Foldr[({type λ[α] = SqlValueT[F, α]})#λ] {
    def foldr[A, B] = k => b => s =>
      implicitly[Foldr[({type λ[α] = EitherT[Err, F, α]})#λ]].foldr(k)(b)(s.value)
  }

  implicit def SqlValueTFoldl[F[_]: Foldl]: Foldl[({type λ[α] = SqlValueT[F, α]})#λ] = new Foldl[({type λ[α] = SqlValueT[F, α]})#λ] {
    def foldl[A, B] = k => b => s =>
      implicitly[Foldl[({type λ[α] = EitherT[Err, F, α]})#λ]].foldl(k)(b)(s.value)
  }

  implicit def SqlValueTTraverse[F[_]: Traverse]: Traverse[({type λ[α] = SqlValueT[F, α]})#λ] =
    implicitly[Traverse[({type λ[α] = EitherT[Err, F, α]})#λ]].xmap[({type λ[α] = SqlValueT[F, α]})#λ](
      new (({type λ[α] = EitherT[Err, F, α]})#λ ~> ({type λ[α] = SqlValueT[F, α]})#λ) {
        def apply[A](a: EitherT[Err, F, A]) = eitherSqlValueT(a)
      }
    , new (({type λ[α] = SqlValueT[F, α]})#λ ~> ({type λ[α] = EitherT[Err, F, α]})#λ) {
        def apply[A](a: SqlValueT[F, A]) = a.value
      }
    )

  implicit def SqlValueTPlus[F[_]](implicit m: ApplicFunctor[F]): Plus[({type λ[α] = SqlValueT[F, α]})#λ] = new Plus[({type λ[α] = SqlValueT[F, α]})#λ] {
    def plus[A](a1: SqlValueT[F, A], a2: => SqlValueT[F, A]) =
      eitherSqlValueT(EitherT(m.liftA2((a1: Either[Err, A]) => (a2: Either[Err, A]) => a1 fold (_ => a2 fold (_ => a1, _ => a2), _ => a1))(a1.value.runT)(a2.value.runT)))
  }

  implicit def SqlValueTEmpty[F[_]](implicit p: Pointed[F]): Empty[({type λ[α] = SqlValueT[F, α]})#λ] = new Empty[({type λ[α] = SqlValueT[F, α]})#λ] {
    def empty[A] = eitherSqlValueT(EitherT(p.point(Left(sqlExceptionContext(new SqlException)))))
  }

  implicit def SqlValueTMonadEmpty[F[_]: Monad]: MonadEmpty[({type λ[α] = SqlValueT[F, α]})#λ] = {
    implicit val p = implicitly[Monad[F]].pointed
    MonadEmpty.monadEmpty[({type λ[α] = SqlValueT[F, α]})#λ]
  }

  implicit def SqlValueTMonadEmptyPlus[F[_]: Monad]: MonadEmptyPlus[({type λ[α] = SqlValueT[F, α]})#λ] = {
    implicit val p = implicitly[Monad[F]].pointed
    implicit val ap = implicitly[Monad[F]].applicFunctor
    MonadEmptyPlus.monadEmptyPlus[({type λ[α] = SqlValueT[F, α]})#λ]
  }

  implicit def SqlValueZero[A]: Zero[SqlValue[A]] =
    Zero.zero(sqlError(sqlExceptionContext(new SqlException)))

  implicit def SqlValueSemigroup[A]: Semigroup[SqlValue[A]] =
    Semigroup.semigroup(a1 => a2 => a1 fold (_ => a2 fold (_ => a1, _ => a2), _ => a1))

  implicit def SqlValueMonoid[A]: Monoid[SqlValue[A]] =
    Monoid.monoid

  implicit def SqlValueShow[A](implicit s: Show[A]): Show[SqlValue[A]] =
    Show.shows(_.fold(
      e => ("sql-error(" + implicitly[Show[Err]].shows(e) + ")")
    , a => ("sql-value(" + s.shows(a) + ")")
    ))

  implicit val SqlValueTMonadTrans: MonadTrans[SqlValueT] = new MonadTrans[SqlValueT] {
    def lift[G[_] : Monad, A](a: G[A]): SqlValueT[G, A] =
      new SqlValueT[G, A] {
        val value = implicitly[MonadTrans[({type λ[α[_], β] = EitherT[Err, α, β]})#λ]].lift(a)
      }
  }

  sealed trait ErrProjectionT[F[_], A] {
    val x: SqlValueT[F, A]

    def foreach(f: Err => Unit)(implicit e: Each[F]): Unit =
      x.value.left foreach f

    def orT(default: => Err)(implicit ftr: Functor[F]): F[Err] =
      x.value.left getOrElseT default

    def or(default: => Err)(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Err =
      x.value.left getOrElse default

    def existsT(f: Err => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
      x.value.left existsT f

    def exists(f: Err => Boolean)(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Boolean =
      x.value.left exists f

    def forallT(f: Err => Boolean)(implicit ftr: Functor[F]): F[Boolean] =
      x.value.left forallT f

    def forall(f: Err => Boolean)(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Boolean =
      x.value.left forall f

    def orElse(y: => SqlValueT[F, A])(implicit m: Bind[F]): SqlValueT[F, A] =
      eitherSqlValueT(x.value.left orElse y.value)

    def optionT(implicit ftr: Functor[F]): OptionT[F, Err] =
      x.value.left toOptionT

    def option(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Option[Err] =
      x.value.left toOption

    def toListT(implicit ftr: Functor[F]): F[List[Err]] =
      x.value.left toListT

    def toList(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): List[Err] =
      x.value.left toList

    def toStreamT(implicit ftr: Functor[F]): F[Stream[Err]] =
      x.value.left toStreamT

    def toStream(implicit i: F[Either[Err, A]] =:= Identity[Either[Err, A]]): Stream[Err] =
      x.value.left toStream
  }

}
