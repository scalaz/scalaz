package scalaz

sealed trait StateT[S, F[_], A] {
  val runT: S => F[(A, S)]

  import StateT._
  import WriterT._

  def *->* : (({type λ[α] = StateT[S, F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = StateT[S, F, α]})#λ, A](this)

  def *->*->* : *->*->*[S, ({type λ[α, β] = StateT[α, F, β]})#λ, A] =
    scalaz.*->*->*.!**->**->**![S, ({type λ[α, β] = StateT[α, F, β]})#λ, A](this)

  def run(s: S)(implicit i: F[(A, S)] =:= Identity[(A, S)]): (A, S) =
    runT(s).value

  def evalT(s: S)(implicit f: Functor[F]): F[A] =
    f.fmap[(A, S), A](_._1)(runT(s))

  def eval(s: S)(implicit i: F[(A, S)] =:= Identity[(A, S)]): A =
    run(s)._1

  def execT(s: S)(implicit f: Functor[F]): F[S] =
    f.fmap[(A, S), S](_._2)(runT(s))

  def exec(s: S)(implicit i: F[(A, S)] =:= Identity[(A, S)]): S =
    run(s)._2

  def usingT: (S => S) => StateT[S, F, A] =
    f => stateT[S, F, A](runT compose f)

  def using(f: S => S)(implicit i: F[(A, S)] =:= Identity[(A, S)]): State[S, A] =
    state[S, A](s => run(f(s)))

  def writerT: S => WriterT[A, F, S] =
    s => WriterT.writerT(runT(s))

  def writer(s: S)(implicit i: F[(A, S)] =:= Identity[(A, S)]): Writer[A, S] =
    WriterT.writer(run(s))

  def map[B](f: A => B)(implicit ftr: Functor[F]): StateT[S, F, B] =
    stateT[S, F, B](s => ftr.fmap((as: (A, S)) => (f(as._1), as._2))(runT(s)))

  def flatMap[B](f: A => StateT[S, F, B])(implicit m: Bind[F]): StateT[S, F, B] =
    stateT[S, F, B](s => m.bind((as: (A, S)) => f(as._1) runT as._2)(runT(s)))
}

object StateT extends StateTs {

  def apply[S, F[_], A](r: S => F[(A, S)]): StateT[S, F, A] =
    stateT(r)
}

trait StateTs {
  type State[S, A] = StateT[S, Identity, A]

  type PartialApplyState[S] =
  PartialApply1Of2[State, S]

  def stateT[S, F[_], A](r: S => F[(A, S)]): StateT[S, F, A] = new StateT[S, F, A] {
    val runT = r
  }

  def state[S, A](r: S => (A, S)): State[S, A] =
    stateT[S, Identity, A](s => Identity.id(r(s)))

  def getT[S, F[_]](implicit p: Pointed[F]): StateT[S, F, S] =
    stateT[S, F, S](s => p.point((s, s)))

  def get[S]: State[S, S] =
    state[S, S](s => (s, s))

  def putT[S, F[_]](s: => S)(implicit p: Pointed[F]): StateT[S, F, Unit] =
    stateT[S, F, Unit](_ => p.point(((), s)))

  def put[S](s: => S): State[S, Unit] =
    state[S, Unit](_ => ((), s))

  def modifyT[S, F[_]](f: S => S)(implicit mnd: Monad[F]): StateT[S, F, Unit] = {
    implicit val p = mnd.pointed
    implicit val b = mnd.bind
    getT[S, F] flatMap (s => putT[S, F](f(s)))
  }

  def modify[S](f: S => S): State[S, Unit] = {
    get[S] flatMap (s => put[S](f(s)))
  }

  implicit def StateTMonadTrans[S]: MonadTrans[({type λ[α[_], β] = StateT[S, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = StateT[S, α, β]})#λ] {
    def lift[G[_] : Monad, A](a: G[A]): StateT[S, G, A] =
      stateT(s => implicitly[Monad[G]].fmap((a: A) => (a, s))(a))
  }

  implicit def StateTFunctor[A, F[_] : Functor]: Functor[({type λ[α] = StateT[A, F, α]})#λ] = new Functor[({type λ[α] = StateT[A, F, α]})#λ] {
    def fmap[X, Y](f: X => Y) =
      _ map f
  }

  implicit def StateTPointed[A, F[_] : Pointed]: Pointed[({type λ[α] = StateT[A, F, α]})#λ] =
    new Pointed[({type λ[α] = StateT[A, F, α]})#λ] {
      def point[A](a: => A) =
        stateT(s => implicitly[Pointed[F]].point((a, s)))
    }

  implicit def StateTJoin[A, F[_] : Bind]: Join[({type λ[α] = StateT[A, F, α]})#λ] = new Join[({type λ[α] = StateT[A, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def StateTBind[A, F[_] : Bind]: Bind[({type λ[α] = StateT[A, F, α]})#λ] = new Bind[({type λ[α] = StateT[A, F, α]})#λ] {
    def bind[X, Y](f: X => StateT[A, F, Y]) =
      _ flatMap f
  }

  implicit def StateTPointedFunctor[A, F[_] : PointedFunctor]: PointedFunctor[({type λ[α] = StateT[A, F, α]})#λ] = new PointedFunctor[({type λ[α] = StateT[A, F, α]})#λ] {
    implicit val ftr = implicitly[PointedFunctor[F]].functor
    implicit val pt = implicitly[PointedFunctor[F]].pointed
    val functor = implicitly[Functor[({type λ[α] = StateT[A, F, α]})#λ]]
    val pointed = implicitly[Pointed[({type λ[α] = StateT[A, F, α]})#λ]]
  }

  implicit def StateTApplic[A, F[_] : Monad]: Applic[({type λ[α] = StateT[A, F, α]})#λ] = new Applic[({type λ[α] = StateT[A, F, α]})#λ] {
    def applic[X, Y](f: StateT[A, F, X => Y]) =
      implicitly[Monad[({type λ[α] = StateT[A, F, α]})#λ]].liftM2[X => Y, X, Y](identity)(f)
  }

  implicit def StateTApplicative[A, F[_]](implicit m: Monad[F]): Applicative[({type λ[α] = StateT[A, F, α]})#λ] = {
    implicit val a = m.applic
    implicit val p = m.pointedFunctor
    Applicative.applicative[({type λ[α] = StateT[A, F, α]})#λ]
  }

  implicit def StateTApplicFunctor[A, F[_]](implicit m: Monad[F]): ApplicFunctor[({type λ[α] = StateT[A, F, α]})#λ] = {
    implicit val a = m.applic
    implicit val f = m.functor
    ApplicFunctor.applicFunctor[({type λ[α] = StateT[A, F, α]})#λ]
  }

  implicit def StateTBindFunctor[A, F[_] : BindFunctor]: BindFunctor[({type λ[α] = StateT[A, F, α]})#λ] = new BindFunctor[({type λ[α] = StateT[A, F, α]})#λ] {
    implicit val ftr = implicitly[BindFunctor[F]].functor
    implicit val b = implicitly[BindFunctor[F]].bind
    val functor = implicitly[Functor[({type λ[α] = StateT[A, F, α]})#λ]]
    val bind = implicitly[Bind[({type λ[α] = StateT[A, F, α]})#λ]]
  }

  implicit def StateTMonad[A, F[_] : Monad]: Monad[({type λ[α] = StateT[A, F, α]})#λ] = {
    implicit val bind = implicitly[Monad[F]].bind
    implicit val pointed = implicitly[Monad[F]].pointed
    Monad.monadBP[({type λ[α] = StateT[A, F, α]})#λ]
  }

}
