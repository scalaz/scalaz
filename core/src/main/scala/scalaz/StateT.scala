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

}
