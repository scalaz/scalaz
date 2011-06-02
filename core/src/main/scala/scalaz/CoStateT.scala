package scalaz

import scala.Predef._

sealed trait CoStateT[A, F[_], B] {
  val runT: (F[A => B], A)

  import CoStateT._

  def *->* : (({type λ[α] = CoStateT[A, F, α]})#λ *->* B) =
    scalaz.*->*.**->**[({type λ[α] = CoStateT[A, F, α]})#λ, B](this)

  def *->*->* : *->*->*[A, ({type λ[α, β] = CoStateT[α, F, β]})#λ, B] =
    scalaz.*->*->*.**->**->**[A, ({type λ[α, β] = CoStateT[α, F, β]})#λ, B](this)

  private def mapRunT[C](f: (A => B) => C)(implicit ftr: Functor[F]): (F[C], A) =
    (ftr.fmap((z: A => B) => f(z))(runT._1), runT._2)

  private def mapRun[C](f: (A => B) => C)(implicit i: F[A => B] =:= Ident[A => B]): (C, A) = {
    val (k, a) = run
    (f(k), a)
  }

  def run(implicit i: F[A => B] =:= Ident[A => B]): (A => B, A) = {
    val (k, a) = runT
    (k.value, a)
  }

  def putT(implicit ftr: Functor[F]): A => F[B] =
    a => ftr.fmap((k: A => B) => k(a))(runT._1)

  def put(implicit i: F[A => B] =:= Ident[A => B]): A => B =
    run._1

  def pos: A =
    runT._2

  def copointT(implicit p: CoPointed[F]): B =
    p.coPoint(runT._1)(runT._2)

  def copoint(implicit i: F[A => B] =:= Ident[A => B]): B =
    run._1(run._2)

  def map[C](f: B => C)(implicit ftr: Functor[F]): CoStateT[A, F, C] =
    coStateT[A, F, C](mapRunT(k => f compose k))

  def duplicateT(implicit p: CoBind[F]): CoStateT[A, F, CoStateT[A, F, B]] =
    coStateT[A, F, CoStateT[A, F, B]]((
        p.coBind((ff: F[A => B]) => (a: A) => coStateT[A, F, B]((ff, a)))(runT._1)
        , pos))

  def duplicate(implicit i: F[A => B] =:= Ident[A => B]): CoState[A, CoState[A, B]] =
    coState[A, CoState[A, B]](
      mapRun[A => CoState[A, B]](k => a =>
        coState[A, B]((k, run._2))))

  def cobindT[C](f: CoStateT[A, F, B] => C)(implicit c: CoBind[F]): CoStateT[A, F, C] =
    coStateT[A, F, C]((
        implicitly[CoBind[F]].coBind((ff: F[A => B]) => (a: A) =>
          f(coStateT[A, F, B]((ff, a)))
        )(runT._1)
        , pos
        ))

  def cobind[C](f: CoState[A, B] => C)(implicit i: F[A => B] =:= Ident[A => B]): CoState[A, C] =
    coState[A, C]((
        (a: A) => f(coState[A, B]((run._1, a)))
        , pos
        ))
}

object CoStateT extends CoStateTs {
  def apply[A, F[_], B](r: (F[A => B], A)): CoStateT[A, F, B] =
    coStateT(r)
}

trait CoStateTs {
  type CoState[A, B] = CoStateT[A, Ident, B]

  type PartialApplyCoState[A] =
  PartialApply1Of2[CoState, A]

  def coStateT[A, F[_], B](r: (F[A => B], A)): CoStateT[A, F, B] = new CoStateT[A, F, B] {
    val runT = r
  }

  def coState[A, B](r: (A => B, A)): CoState[A, B] =
    coStateT[A, Ident, B](Ident.ident(r._1), r._2)

  implicit def CoStateTCoMonadTrans[S]: CoMonadTrans[({type λ[α[_], β] = CoStateT[S, α, β]})#λ] = new CoMonadTrans[({type λ[α[_], β] = CoStateT[S, α, β]})#λ] {
    def lower[G[_] : Extend, A](a: CoStateT[S, G, A]) =
      implicitly[Extend[G]].fmap((z: S => A) => z(a.runT._2))(a.runT._1)
  }

  implicit def CoStateFunctor[A, F[_] : Functor]: Functor[({type λ[α] = CoStateT[A, F, α]})#λ] = new Functor[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def CoStatePointed[A: Zero, F[_] : Pointed]: Pointed[({type λ[α] = CoStateT[A, F, α]})#λ] = new Pointed[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def point[Z](z: => Z) =
      coStateT[A, F, Z]((implicitly[Pointed[F]].point(_ => z), implicitly[Zero[A]].zero))
  }

  implicit def CoStatePointedFunctor[A: Zero, F[_] : PointedFunctor]: PointedFunctor[({type λ[α] = CoStateT[A, F, α]})#λ] = new PointedFunctor[({type λ[α] = CoStateT[A, F, α]})#λ] {
    implicit val ftr = implicitly[PointedFunctor[F]].functor
    implicit val pt = implicitly[PointedFunctor[F]].pointed
    val functor = implicitly[Functor[({type λ[α] = CoStateT[A, F, α]})#λ]]
    val pointed = implicitly[Pointed[({type λ[α] = CoStateT[A, F, α]})#λ]]
  }

  implicit def CoStateApplic[A: Semigroup, F[_] : ApplicFunctor]: Applic[({type λ[α] = CoStateT[A, F, α]})#λ] = new Applic[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def applic[X, Y](f: CoStateT[A, F, X => Y]) =
      a => coStateT[A, F, Y]((
          implicitly[ApplicFunctor[F]].liftA2((ff: A => X => Y) => (aa: A => X) => (z: A) => ff(z)(aa(z)))(f.runT._1)(a.runT._1), implicitly[Semigroup[A]].append(f.pos, a.pos)
          ))
  }

  implicit def CoStateApplicFunctor[A: Semigroup, F[_] : ApplicFunctor]: ApplicFunctor[({type λ[α] = CoStateT[A, F, α]})#λ] = new ApplicFunctor[({type λ[α] = CoStateT[A, F, α]})#λ] {
    implicit val ftr = implicitly[ApplicFunctor[F]].functor
    val functor = implicitly[Functor[({type λ[α] = CoStateT[A, F, α]})#λ]]
    val applic = implicitly[Applic[({type λ[α] = CoStateT[A, F, α]})#λ]]
  }

  implicit def CoStateApplicative[A: Monoid, F[_] : Applicative]: Applicative[({type λ[α] = CoStateT[A, F, α]})#λ] = new Applicative[({type λ[α] = CoStateT[A, F, α]})#λ] {
    implicit val s = implicitly[Monoid[A]].semigroup
    implicit val z = implicitly[Monoid[A]].zero
    implicit val ap = implicitly[Applicative[F]].applic
    implicit val pf = implicitly[Applicative[F]].pointedFunctor
    implicit val af = implicitly[Applicative[F]].applicFunctor
    val pointedFunctor = implicitly[PointedFunctor[({type λ[α] = CoStateT[A, F, α]})#λ]]
    val applic = implicitly[Applic[({type λ[α] = CoStateT[A, F, α]})#λ]]

  }

  implicit def CoStateCoBind[A, F[_] : CoBind]: CoBind[({type λ[α] = CoStateT[A, F, α]})#λ] = new CoBind[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def coBind[X, Y](f: CoStateT[A, F, X] => Y) =
      _ cobindT f
  }

  implicit def CoStateCoPointed[A, F[_] : CoPointed]: CoPointed[({type λ[α] = CoStateT[A, F, α]})#λ] = new CoPointed[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def coPoint[X] =
      _.copointT
  }

  implicit def CoStateCoJoin[A, F[_] : CoBind]: CoJoin[({type λ[α] = CoStateT[A, F, α]})#λ] = new CoJoin[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def coJoin[X] =
      _.duplicateT
  }

  implicit def CoStateExtend[A, F[_] : Extend]: Extend[({type λ[α] = CoStateT[A, F, α]})#λ] = new Extend[({type λ[α] = CoStateT[A, F, α]})#λ] {
    implicit val ftr = implicitly[Extend[F]].functor
    implicit val cb = implicitly[Extend[F]].coBind
    val functor = implicitly[Functor[({type λ[α] = CoStateT[A, F, α]})#λ]]
    val coJoin = implicitly[CoJoin[({type λ[α] = CoStateT[A, F, α]})#λ]]
  }

  implicit def CoStateCoMonad[A, F[_] : CoMonad]: CoMonad[({type λ[α] = CoStateT[A, F, α]})#λ] = new CoMonad[({type λ[α] = CoStateT[A, F, α]})#λ] {
    implicit val cb = implicitly[CoMonad[F]].coBind
    implicit val cp = implicitly[CoMonad[F]].coPointed
    implicit val ftr = implicitly[CoMonad[F]].functor
    val coBind = implicitly[CoBind[({type λ[α] = CoStateT[A, F, α]})#λ]]
    val coPointed = implicitly[CoPointed[({type λ[α] = CoStateT[A, F, α]})#λ]]
    val functor = implicitly[Functor[({type λ[α] = CoStateT[A, F, α]})#λ]]
    val coJoin = implicitly[CoJoin[({type λ[α] = CoStateT[A, F, α]})#λ]]
  }

}