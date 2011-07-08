package scalaz
package sql

import SqlValueT._

sealed trait SqlValueT[F[_], A] {
  val value: EitherT[SqlException, F, A]

  import SqlValueT._

  def map[B](f: A => B)(implicit ftr: Functor[F]): SqlValueT[F, B] =
    eitherSqlValueT(value map f)

  def flatMap[B](f: A => SqlValueT[F, B])(implicit m: Monad[F]): SqlValueT[F, B] =
    eitherSqlValueT(value flatMap (f(_).value))

}

object SqlValueT extends SqlValueTs

trait SqlValueTs {
  type SqlException =
  java.sql.SQLException

  type SqlValue[A] =
  SqlValueT[Identity, A]

  def eitherSqlValueT[F[_], A](a: EitherT[SqlException, F, A]): SqlValueT[F, A] = new SqlValueT[F, A] {
    val value = a
  }

  def sqlValueT[F[_], A](a: F[A])(implicit ftr: Functor[F]): SqlValueT[F, A] = new SqlValueT[F, A] {
    val value = EitherT.eitherT(ftr.fmap((a: A) => Right(a): Either[SqlException, A])(a))
  }

  def sqlErrorT[F[_], A](a: F[SqlException])(implicit ftr: Functor[F]): SqlValueT[F, A] = new SqlValueT[F, A] {
    val value = EitherT.eitherT(ftr.fmap((e: SqlException) => Left(e): Either[SqlException, A])(a))
  }

  def sqlValue[A]: A => SqlValue[A] =
    a => sqlValueT(Identity.id(a))

  def sqlError[A]: SqlException => SqlValue[A] =
    e => sqlErrorT(Identity.id(e))

  implicit def SqlValueTFunctor[F[_]: Functor]: Functor[({type λ[α] = SqlValueT[F, α]})#λ] = new Functor[({type λ[α] = SqlValueT[F, α]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def SqlValueTApplic[F[_]: ApplicFunctor]: Applic[({type λ[α] = SqlValueT[F, α]})#λ] = new Applic[({type λ[α] = SqlValueT[F, α]})#λ] {
    def applic[A, B](f: SqlValueT[F, A => B]) =
      a => eitherSqlValueT(implicitly[Applic[({type λ[α] = EitherT[SqlException, F, α]})#λ]].applic(f.value)(a.value))
  }

  implicit def SqlValueTBind[F[_]: Monad]: Bind[({type λ[α] = SqlValueT[F, α]})#λ] = new Bind[({type λ[α] = SqlValueT[F, α]})#λ] {
    def bind[A, B](f: A => SqlValueT[F, B]) =
      _ flatMap f
  }

  implicit def SqlValueTPointed[F[_]: Pointed]: Pointed[({type λ[α] = SqlValueT[F, α]})#λ] = new Pointed[({type λ[α] = SqlValueT[F, α]})#λ] {
    def point[A](a: => A) =
      eitherSqlValueT(implicitly[Pointed[({type λ[α] = EitherT[SqlException, F, α]})#λ]].point(a))
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

  implicit def SqlValueTFoldr[F[_]: Foldr]: Foldr[({type λ[α] = SqlValueT[F, α]})#λ] = new Foldr[({type λ[α] = SqlValueT[F, α]})#λ] {
    def foldr[A, B] = k => b => s =>
      implicitly[Foldr[({type λ[α] = EitherT[SqlException, F, α]})#λ]].foldr(k)(b)(s.value)
  }

  implicit def SqlValueTFoldl[F[_]: Foldl]: Foldl[({type λ[α] = SqlValueT[F, α]})#λ] = new Foldl[({type λ[α] = SqlValueT[F, α]})#λ] {
    def foldl[A, B] = k => b => s =>
      implicitly[Foldl[({type λ[α] = EitherT[SqlException, F, α]})#λ]].foldl(k)(b)(s.value)
  }

  implicit def SqlValueTTraverse[F[_]: Traverse]: Traverse[({type λ[α] = SqlValueT[F, α]})#λ] =
    implicitly[Traverse[({type λ[α] = EitherT[SqlException, F, α]})#λ]].xmap[({type λ[α] = SqlValueT[F, α]})#λ](
      new (({type λ[α] = EitherT[SqlException, F, α]})#λ ~> ({type λ[α] = SqlValueT[F, α]})#λ) {
        def apply[A](a: EitherT[SqlException, F, A]) = eitherSqlValueT(a)
      }
    , new (({type λ[α] = SqlValueT[F, α]})#λ ~> ({type λ[α] = EitherT[SqlException, F, α]})#λ) {
        def apply[A](a: SqlValueT[F, A]) = a.value
      }
    )

  implicit val SqlValueTMonadTrans: MonadTrans[SqlValueT] = new MonadTrans[SqlValueT] {
    def lift[G[_] : Monad, A](a: G[A]): SqlValueT[G, A] =
      new SqlValueT[G, A] {
        val value = implicitly[MonadTrans[({type λ[α[_], β] = EitherT[SqlException, α, β]})#λ]].lift(a)
      }
  }

}
