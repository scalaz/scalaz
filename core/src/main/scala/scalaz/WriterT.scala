package scalaz

sealed trait WriterT[W, F[_], A] {
  val runT: F[(W, A)]

  import WriterT._

  def *->* : (({type λ[α] = WriterT[W, F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = WriterT[W, F, α]})#λ, A](this)

  def *->*->* : *->*->*[W, ({type λ[α, β] = WriterT[α, F, β]})#λ, A] =
    scalaz.*->*->*.!**->**->**![W, ({type λ[α, β] = WriterT[α, F, β]})#λ, A](this)

  def run(implicit i: F[(W, A)] =:= Identity[(W, A)]): (W, A) =
    runT.value

  def mapValue[X, B](f: ((W, A)) => (X, B))(implicit ftr: Functor[F]): WriterT[X, F, B] =
    writerT(ftr.fmap(f)(runT))

  def mapWritten[X](f: W => X)(implicit ftr: Functor[F]): WriterT[X, F, A] =
    mapValue(wa => (f(wa._1), wa._2))

  def writtenT(implicit ftr: Functor[F]): F[W] =
    ftr.fmap((_: (W, A))._1)(runT)

  def written(implicit i: F[(W, A)] =:= Identity[(W, A)]): W =
    run._1

  def overT(implicit ftr: Functor[F]): F[A] =
    ftr.fmap((_: (W, A))._2)(runT)

  def over(implicit i: F[(W, A)] =:= Identity[(W, A)]): A =
    run._2

  def swapT(implicit ftr: Functor[F]): WriterT[A, F, W] =
    mapValue(wa => (wa._2, wa._1))

  def swap(implicit i: F[(W, A)] =:= Identity[(W, A)]): Writer[A, W] = {
    val (w, a) = run
    writer((a, w))
  }

  def :++>(w: => W)(implicit f: Functor[F], s: Semigroup[W]): WriterT[W, F, A] =
    mapWritten(s.append(_, w))

  def :++>>(f: A => W)(implicit ftr: Functor[F], s: Semigroup[W]): WriterT[W, F, A] =
    mapValue(wa => (s.append(wa._1, f(wa._2)), wa._2))

  def <++:(w: => W)(implicit f: Functor[F], s: Semigroup[W]): WriterT[W, F, A] =
    mapWritten(s.append(w, _))

  def <<++:(f: A => W)(implicit ftr: Functor[F], s: Semigroup[W]): WriterT[W, F, A] =
    mapValue(wa => (s.append(f(wa._2), wa._1), wa._2))

  def reset(implicit z: Zero[W], f: Functor[F]): WriterT[W, F, A] =
    mapWritten(_ => z.zero)

  def map[B](f: A => B)(implicit ftr: Functor[F]): WriterT[W, F, B] =
    writerT(ftr.fmap((wa: (W, A)) => (wa._1, f(wa._2)))(runT))

  def foreach[B](f: A => Unit)(implicit e: Each[F]): Unit =
    e.each((wa: (W, A)) => f(wa._2))(runT)

  def flatMap[B](f: A => WriterT[W, F, B])(implicit b: BindFunctor[F], s: Semigroup[W]): WriterT[W, F, B] =
    writerT(b.bind((wa: (W, A)) => {
      val z = f(wa._2).runT
      b.fmap((wb: (W, B)) => (s.append(wa._1, wb._1), wb._2))(z)
    })(runT))
}

object WriterT extends WriterTs {
  def apply[W, F[_], A](v: F[(W, A)]): WriterT[W, F, A] =
    writerT(v)
}

trait WriterTs {
  type Writer[W, A] = WriterT[W, Identity, A]

  def writerT[W, F[_], A](v: F[(W, A)]): WriterT[W, F, A] = new WriterT[W, F, A] {
    val runT = v
  }

  def writer[W, A](v: (W, A)): Writer[W, A] =
    writerT(Identity.id(v))

  implicit def WriterTMonadTrans[W: Zero]: MonadTrans[({type λ[α[_], β] = WriterT[W, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = WriterT[W, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): WriterT[W, G, A] =
      writerT(implicitly[Monad[G]].fmap((a: A) => (implicitly[Zero[W]].zero, a))(a))
  }

  implicit def WriterTFunctor[A, F[_] : Functor]: Functor[({type λ[α] = WriterT[A, F, α]})#λ] = new Functor[({type λ[α] = WriterT[A, F, α]})#λ] {
    def fmap[X, Y](f: X => Y) =
      _ map f
  }

  implicit def WriterTPointed[A: Zero, F[_] : Pointed]: Pointed[({type λ[α] = WriterT[A, F, α]})#λ] =
    new Pointed[({type λ[α] = WriterT[A, F, α]})#λ] {
      def point[X](a: => X) =
        writerT(implicitly[Pointed[F]].point((implicitly[Zero[A]].zero, a)))
    }

  implicit def WriterTJoin[A: Semigroup, F[_] : BindFunctor]: Join[({type λ[α] = WriterT[A, F, α]})#λ] = new Join[({type λ[α] = WriterT[A, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def WriterTBind[A: Semigroup, F[_] : BindFunctor]: Bind[({type λ[α] = WriterT[A, F, α]})#λ] = new Bind[({type λ[α] = WriterT[A, F, α]})#λ] {
    def bind[X, Y](f: X => WriterT[A, F, Y]) =
      _ flatMap f
  }


  implicit def WriterTPointedFunctor[A: Zero, F[_] : PointedFunctor]: PointedFunctor[({type λ[α] = WriterT[A, F, α]})#λ] = new PointedFunctor[({type λ[α] = WriterT[A, F, α]})#λ] {
    implicit val ftr = implicitly[PointedFunctor[F]].functor
    implicit val pt = implicitly[PointedFunctor[F]].pointed
    val functor = implicitly[Functor[({type λ[α] = WriterT[A, F, α]})#λ]]
    val pointed = implicitly[Pointed[({type λ[α] = WriterT[A, F, α]})#λ]]
  }

  implicit def WriterTApplic[A: Semigroup, F[_] : ApplicFunctor]: Applic[({type λ[α] = WriterT[A, F, α]})#λ] = new Applic[({type λ[α] = WriterT[A, F, α]})#λ] {
    def applic[X, Y](f: WriterT[A, F, X => Y]) =
      a =>
        writerT(implicitly[ApplicFunctor[F]].liftA2((ff: (A, X => Y)) => (xx: (A, X)) => (implicitly[Semigroup[A]].append(ff._1, xx._1), ff._2(xx._2)))(f.runT)(a.runT))
  }

  implicit def WriterTApplicative[A, F[_]](implicit ap: Applicative[F], n: Monoid[A]): Applicative[({type λ[α] = WriterT[A, F, α]})#λ] = {
    implicit val a = ap.applic
    implicit val p = ap.pointedFunctor
    implicit val f = ap.applicFunctor
    implicit val s = n.semigroup
    implicit val z = n.zero
    Applicative.applicative[({type λ[α] = WriterT[A, F, α]})#λ]
  }

  implicit def WriterTApplicFunctor[A, F[_]](implicit ap: ApplicFunctor[F], n: Semigroup[A]): ApplicFunctor[({type λ[α] = WriterT[A, F, α]})#λ] = {
    implicit val a = ap.applic
    implicit val f = ap.functor
    ApplicFunctor.applicFunctor[({type λ[α] = WriterT[A, F, α]})#λ]
  }

  implicit def WriterTBindFunctor[A: Semigroup, F[_] : BindFunctor]: BindFunctor[({type λ[α] = WriterT[A, F, α]})#λ] = new BindFunctor[({type λ[α] = WriterT[A, F, α]})#λ] {
    implicit val ftr = implicitly[BindFunctor[F]].functor
    val functor = implicitly[Functor[({type λ[α] = WriterT[A, F, α]})#λ]]
    val bind = implicitly[Bind[({type λ[α] = WriterT[A, F, α]})#λ]]
  }

  implicit def WriterTMonad[A, F[_]](implicit m: Monad[F], n: Monoid[A]): Monad[({type λ[α] = WriterT[A, F, α]})#λ] = {
    implicit val bindF = implicitly[Monad[F]].bindFunctor
    implicit val pointed = implicitly[Monad[F]].pointed
    implicit val s = n.semigroup
    implicit val z = n.zero
    Monad.monadBP[({type λ[α] = WriterT[A, F, α]})#λ]
  }
}