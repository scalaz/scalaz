package scalaz

import annotation.tailrec
import FreeT._

object FreeT extends FreeTInstances {
  /** Suspend the computation with the given suspension. */
  private case class Suspend[S[_], M[_], A](a: M[A \/ S[FreeT[S, M, A]]]) extends FreeT[S, M, A]

  /** Call a subroutine and continue with the given function. */
  private sealed abstract case class Gosub[S[_], M[_], B]() extends FreeT[S, M, B] {
    type C
    val a: FreeT[S, M, C]
    val f: C => FreeT[S, M, B]
  }

  def gosub[S[_], M[_], B, C0](a0: FreeT[S, M, C0])(f0: C0 => FreeT[S, M, B]): FreeT[S, M, B] =
    new Gosub[S, M, B] {
      override type C = C0
      override val a = a0
      override val f = f0
    }

  /** Return the given value in the free monad. */
  def point[S[_], M[_], A](value: A)(implicit M: Applicative[M]): FreeT[S, M, A] = Suspend(M.point(-\/(value)))

  def suspend[S[_], M[_], A](a: M[A \/ S[FreeT[S, M, A]]]): FreeT[S, M, A] = Suspend(a)

  def tailrecM[S[_], M[_]: Applicative, A, B](f: A => FreeT[S, M, A \/ B])(a: A): FreeT[S, M, B] =
    f(a).flatMap {
      case -\/(a0) => tailrecM(f)(a0)
      case \/-(b) => point[S, M, B](b)
    }

  def liftM[S[_], M[_], A](value: M[A])(implicit M: Functor[M]): FreeT[S, M, A] =
    Suspend(M.map(value)(\/.left))

  /** A version of `liftM` that infers the nested type constructor. */
  def liftMU[S[_], MA](value: MA)(implicit M: Unapply[Functor, MA]): FreeT[S, M.M, M.A] =
    liftM[S, M.M, M.A](M(value))(M.TC)

  /** Suspends a value within a functor in a single step. Monadic unit for a higher-order monad. */
  def liftF[S[_], M[_], A](value: S[A])(implicit S: Functor[S], M: Applicative[M]): FreeT[S, M, A] =
    Suspend(M.point(\/-(S.map(value)(point[S, M, A]))))

  def roll[S[_], M[_], A](value: S[FreeT[S, M, A]])(implicit S: Functor[S], M: Applicative[M]): FreeT[S, M, A] =
    liftF[S, M, FreeT[S, M, A]](value).flatMap(identity)

  import Isomorphism._

  def isoFree[S[_]](implicit S: Functor[S]): FreeT[S, Id.Id, ?] <~> Free[S, ?] =
    new IsoFunctorTemplate[FreeT[S, Id.Id, ?], Free[S, ?]] {
      override def to[A](fa: FreeT[S, Id.Id, A]) = fa match {
        case Suspend(\/-(a)) =>
          Free.roll(S.map(a)(to(_)))
        case Suspend(-\/(a)) =>
          Free.point(a)
        case a @ Gosub() =>
          to(a.a).flatMap(a.f.andThen(to(_)))
      }
      override def from[A](ga: Free[S, A]) =
        ga.toFreeT
    }
}

sealed abstract class FreeT[S[_], M[_], A] {
  final def map[B](f: A => B)(implicit S: Functor[S], M: Functor[M]): FreeT[S, M, B] =
    this match {
      case Suspend(m) => Suspend(M.map(m)(_.bimap(f, S.lift(_.map(f)))))
      case g @ Gosub() => gosub(g.a)(g.f.andThen(_.map(f)))
    }

  /** Binds the given continuation to the result of this computation. */
  final def flatMap[B](f: A => FreeT[S, M, B]): FreeT[S, M, B] =
    this match {
      case g @ Gosub() => gosub(g.a)(x => gosub(g.f(x))(f))
      case a => gosub(a)(f)
    }

  /**
   * Changes the underlying `Monad` for this `FreeT`, ie.
   * turning this `FreeT[S, M, A]` into a `FreeT[S, N, A]`
   * given Functors for `S` and `N`
   */
  def hoistN[N[_]](mn: M ~> N)(implicit S: Functor[S], N: Functor[N]): FreeT[S, N, A] =
    step match {
      case e @ Gosub() =>
        gosub(e.a.hoistN(mn))(e.f.andThen(_.hoistN(mn)))
      case Suspend(m) =>
        Suspend(N.map(mn(m))(_.map(s => S.map(s)(_.hoistN(mn)))))
    }

  /** Same as `hoistN` but different constraints */
  def hoistM[N[_]](mn: M ~> N)(implicit S: Functor[S], M: Functor[M]): FreeT[S, N, A] =
    step match {
      case e @ Gosub() =>
        gosub(e.a.hoistM(mn))(e.f.andThen(_.hoistM(mn)))
      case Suspend(m) =>
        Suspend(mn(M.map(m)(_.map(s => S.map(s)(_.hoistM(mn))))))
    }

  /** Change the base functor `S` for a `FreeT` action. */
  def interpretS[T[_]](st: S ~> T)(implicit S: Functor[S], M: Functor[M]): FreeT[T, M, A] =
    step match {
      case e @ Gosub() =>
        gosub(e.a.interpretS(st))(e.f.andThen(_.interpretS(st)))
      case Suspend(m) =>
        Suspend(M.map(m)(_.map(s => st(S.map(s)(_.interpretS(st))))))
    }

  /** Same as `interpretS` but different constraints */
  def interpretT[T[_]](st: S ~> T)(implicit T: Functor[T], M: Functor[M]): FreeT[T, M, A] =
    step match {
      case e @ Gosub() =>
        gosub(e.a.interpretT(st))(e.f.andThen(_.interpretT(st)))
      case Suspend(m) =>
        Suspend(M.map(m)(_.map(s => T.map(st(s))(_.interpretT(st)))))
    }

  /** Evaluates a single layer of the free monad **/
  def resume(implicit S: Functor[S], M0: BindRec[M], M1: Applicative[M]): M[A \/ S[FreeT[S, M, A]]] = {
    def go(ft: FreeT[S, M, A]): M[FreeT[S, M, A] \/ (A \/ S[FreeT[S, M, A]])] =
      ft match {
        case Suspend(f) => M0.map(f)(\/.right)
        case g1 @ Gosub() => g1.a match {
          case Suspend(m1) => M0.map(m1) {
            case -\/(a) => -\/(g1.f(a))
            case \/-(fc) => \/-(\/-(S.map(fc)(_.flatMap(g1.f))))
          }
          case g2 @ Gosub() => M1.point(-\/(g2.a.flatMap(g2.f(_).flatMap(g1.f))))
        }
      }

    M0.tailrecM(go)(this)
  }

  /**
    * Runs to completion, using a function that maps the resumption from `S` to a monad `M`.
    */
  def runM(interp: S[FreeT[S, M, A]] => M[FreeT[S, M, A]])(implicit S: Functor[S], M0: BindRec[M], M1: Applicative[M]): M[A] = {
    def runM2(ft: FreeT[S, M, A]): M[FreeT[S, M, A] \/ A] =
      M0.bind(ft.resume) {
        case -\/(a) => M1.point(\/-(a))
        case \/-(fc) => M0.map(interp(fc))(\/.left)
      }

    M0.tailrecM(runM2)(this)
  }

  @tailrec
  private def step: FreeT[S, M, A] =
    this match {
      case g @ Gosub() => g.a match {
        case g0 @ Gosub() => g0.a.flatMap(a => g0.f(a).flatMap(g.f)).step
        case _ => g
      }
      case x => x
    }
}

sealed abstract class FreeTInstances6 {
  implicit def freeTMonadTell[S[_]: Functor, M[_], E](implicit M1: MonadTell[M, E]): MonadTell[FreeT[S, M, ?], E] =
    new MonadTell[FreeT[S, M, ?], E] with FreeTMonad[S, M] {
      override def S = implicitly
      override def M = implicitly
      override def writer[A](w: E, v: A) =
        FreeT.liftM(M1.writer(w, v))
    }
}

sealed abstract class FreeTInstances5 extends FreeTInstances6 {
  implicit def freeTMonadReader[S[_]: Functor, M[_], E](implicit M1: MonadReader[M, E]): MonadReader[FreeT[S, M, ?], E] =
    new MonadReader[FreeT[S, M, ?], E] with FreeTMonad[S, M] {
      override def S = implicitly
      override def M = implicitly
      override def ask =
        FreeT.liftM(M1.ask)
      override def local[A](f: E => E)(fa: FreeT[S, M, A]) =
        fa.hoistM(λ[M ~> M](M1.local(f)(_)))
    }
}

sealed abstract class FreeTInstances4 extends FreeTInstances5 {
  implicit def freeTMonadState[S[_]: Functor, M[_], E](implicit M1: MonadState[M, E]): MonadState[FreeT[S, M, ?], E] =
    new MonadState[FreeT[S, M, ?], E] with FreeTMonad[S, M] {
      override def S = implicitly
      override def M = implicitly
      override def init =
        FreeT.liftM(M1.init)
      override def get =
        FreeT.liftM(M1.get)
      override def put(s: E) =
        FreeT.liftM(M1.put(s))
    }
}

sealed abstract class FreeTInstances3 extends FreeTInstances4 {
  implicit def freeTMonadError[S[_]: Functor, M[_]: BindRec, E](implicit E: MonadError[M, E]): MonadError[FreeT[S, M, ?], E] =
    new MonadError[FreeT[S, M, ?], E] with FreeTMonad[S, M] {
      override def S = implicitly
      override def M = implicitly
      override def handleError[A](fa: FreeT[S, M, A])(f: E => FreeT[S, M, A]) =
        FreeT.suspend(E.handleError(fa.resume)(f.andThen(_.resume)))
      override def raiseError[A](e: E) =
        FreeT.liftM(E.raiseError[A](e))(M)
    }
}

sealed abstract class FreeTInstances2 extends FreeTInstances3 {
  implicit def freeTBind[S[_], M[_]](implicit S0: Functor[S], M0: Functor[M]): Bind[FreeT[S, M, ?]] =
    new FreeTBind[S, M] {
      implicit def S: Functor[S] = S0
      implicit def M: Functor[M] = M0
    }

  implicit def freeTHoist[S[_]: Functor]: Hoist[FreeT[S, ?[_], ?]] =
    new Hoist[FreeT[S, ?[_], ?]] {
      def hoist[M[_]: Monad, N[_]](f: M ~> N) =
        λ[FreeT[S, M, ?] ~> FreeT[S, N, ?]](_ hoistM f)
      def liftM[G[_]: Monad, A](a: G[A]) =
        FreeT.liftM(a)
      def apply[G[_]: Monad] =
        Monad[FreeT[S, G, ?]]
    }

  implicit def freeTFoldable[S[_]: Foldable: Functor, M[_]: Foldable: Applicative: BindRec]: Foldable[FreeT[S, M, ?]] =
    new FreeTFoldable[S, M] {
      override def S = implicitly
      override def F = implicitly
      override def M = implicitly
      override def M1 = implicitly
      override def M2 = implicitly
    }
}

sealed abstract class FreeTInstances1 extends FreeTInstances2 {
  implicit def freeTTraverse[S[_]: Traverse, M[_]: Traverse: Applicative: BindRec]: Traverse[FreeT[S, M, ?]] =
    new FreeTTraverse[S, M] {
      override def F = implicitly
      override def M = implicitly
      override def M1 = implicitly
      override def M2 = implicitly
    }
}

sealed abstract class FreeTInstances0 extends FreeTInstances1 {
  implicit def freeTMonad[S[_], M[_]](implicit S0: Functor[S], M0: Applicative[M]): Monad[FreeT[S, M, ?]] with BindRec[FreeT[S, M, ?]] =
    new FreeTMonad[S, M] {
      def S = S0
      def M = M0
    }

  implicit def freeTPlus[S[_]: Functor, M[_]: Applicative: BindRec: Plus]: Plus[FreeT[S, M, ?]] =
    new FreeTPlus[S, M] {
      override def S = implicitly
      override def M = implicitly
      override def M1 = implicitly
      override def M2 = implicitly
    }
}

sealed abstract class FreeTInstances extends FreeTInstances0 {
  implicit def freeTMonadPlus[S[_]: Functor, M[_]: ApplicativePlus: BindRec]: MonadPlus[FreeT[S, M, ?]] =
    new MonadPlus[FreeT[S, M, ?]] with FreeTPlus[S, M] with FreeTMonad[S, M] {
      override def S = implicitly
      override def M = implicitly
      override def M1 = implicitly
      override def M2 = implicitly

      override def empty[A] = FreeT.liftM[S, M, A](PlusEmpty[M].empty[A])(M)
    }
}

private trait FreeTBind[S[_], M[_]] extends Bind[FreeT[S, M, ?]] {
  implicit def S: Functor[S]
  implicit def M: Functor[M]

  override final def map[A, B](fa: FreeT[S, M, A])(f: A => B): FreeT[S, M, B] = fa.map(f)
  def bind[A, B](fa: FreeT[S, M, A])(f: A => FreeT[S, M, B]): FreeT[S, M, B] = fa.flatMap(f)
}

private trait FreeTMonad[S[_], M[_]] extends Monad[FreeT[S, M, ?]] with BindRec[FreeT[S, M, ?]] with FreeTBind[S, M] {
  implicit def M: Applicative[M]

  override final def point[A](a: => A) =
    FreeT.point[S, M, A](a)
  override final def tailrecM[A, B](f: A => FreeT[S, M, A \/ B])(a: A) =
    FreeT.tailrecM(f)(a)
}

private trait FreeTPlus[S[_], M[_]] extends Plus[FreeT[S, M, ?]] {
  implicit def S: Functor[S]
  implicit def M: Applicative[M]
  implicit def M1: BindRec[M]
  def M2: Plus[M]
  override final def plus[A](a: FreeT[S, M, A], b: => FreeT[S, M, A]) =
    FreeT.suspend(M2.plus(a.resume, b.resume))
}

private trait FreeTFoldable[S[_], M[_]] extends Foldable[FreeT[S, M, ?]] with Foldable.FromFoldMap[FreeT[S, M, ?]] {
  implicit def S: Functor[S]
  implicit def M: Applicative[M]
  implicit def M1: BindRec[M]
  def F: Foldable[S]
  def M2: Foldable[M]

  override final def foldMap[A, B: Monoid](fa: FreeT[S, M, A])(f: A => B): B =
    M2.foldMap(fa.resume){
      case \/-(a) =>
        F.foldMap(a)(foldMap(_)(f))
      case -\/(a) =>
        f(a)
    }
}

private trait FreeTTraverse[S[_], M[_]] extends Traverse[FreeT[S, M, ?]] with FreeTFoldable[S, M] with FreeTBind[S, M] {
  override final def S: Functor[S] = F
  override implicit def F: Traverse[S]
  override def M2: Traverse[M]
  override implicit def M: Applicative[M]
  override implicit def M1: BindRec[M]

  override final def traverseImpl[G[_], A, B](fa: FreeT[S, M, A])(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(
      M2.traverseImpl(fa.resume){
        case \/-(a) =>
          G.map(F.traverseImpl(a)(traverseImpl(_)(f)))(FreeT.roll(_)(S, M))
        case -\/(a) =>
          G.map(f(a))(FreeT.point[S, M, B])
      }
    )(FreeT.liftM(_)(M).flatMap(identity))
}
