package scalaz
package data

sealed trait StepListT[F[_], A] {

  import StepListT._
  import StateT._
  import Ident._

  protected def step: F[Step[A, StepListT[F, A]]]

  def *->* : (({type λ[α] = StepListT[F, α]})#λ *->* A) =
    data.*->*.**->**[({type λ[α] = StepListT[F, α]})#λ, A](this)

  private def stepFlatMap[B](k: Step[A, StepListT[F, A]] => F[B])(implicit m: Monad[F]): F[B] =
    m.bd(k)(step)

  private def stepMap[B](k: Step[A, StepListT[F, A]] => B)(implicit m: Functor[F]): F[B] =
    m.fmap(k)(step)

  def runList[S](s: S)(implicit i: F[Step[A, StepListT[F, A]]] =:= PartialApplyState[S]#Apply[Step[A, StepListT[PartialApplyState[S]#Apply, A]]]): StepList[A] = {
    val r = step.run
    listT(ident(r(s) match {
      case (Yield(a, as), s1) => Yield[A, StepList[A]](a, as runList s1)
      case (Skip(as), s1) => Skip(as runList s1)
      case (Done(), _) => Done()
    }))
  }

  def uncons(implicit m: Monad[F]): F[Option[(A, StepListT[F, A])]] =
    stepFlatMap {
      case Yield(a, s) => m.point(Some((a, s)))
      case Skip(s) => s.uncons
      case Done() => m.point(None)
    }

  def ::(a: => A)(implicit m: Pointed[F]): StepListT[F, A] =
    listT[F, A](m.point(Yield(a, this)))

  def isEmpty(implicit m: Monad[F]): F[Boolean] =
    m.fmap((_: Option[(A, StepListT[F, A])]).isDefined)(uncons)

  def head(implicit m: Monad[F]): F[A] =
    m.fmap((_: Option[(A, StepListT[F, A])]).getOrElse(sys.error("head: empty StepListT"))._1)(uncons)

  /**Don't use iteratively! */
  def tail(implicit m: Functor[F]): StepListT[F, A] =
    listT[F, A](
      stepMap {
        case Yield(a, s) => Skip(s)
        case Skip(s) => Skip(s.tail)
        case Done() => sys.error("tail: empty StepListT")
      })

  def tailM(implicit m: Monad[F]): F[StepListT[F, A]] =
    m.fmap((_: Option[(A, StepListT[F, A])]).getOrElse(sys.error("tailM: empty StepListT"))._2)(uncons)

  def filter(p: A => Boolean)(implicit m: Functor[F]): StepListT[F, A] =
    listT[F, A](
      stepMap {
        case Yield(a, as) => if (p(a)) Yield(a, as filter p) else Skip(as filter p)
        case Skip(as) => Skip(as filter p)
        case Done() => Done()
      })

  def trans[G[_]](t: F ~> G)(implicit m: Functor[G]): StepListT[G, A] =
    listT[G, A](
      m.fmap((x: Step[A, StepListT[F, A]]) => x match {
        case Yield(a, as) => Yield[A, StepListT[G, A]](a, as trans t): Step[A, StepListT[G, A]]
        case Skip(as) => Skip[A, StepListT[G, A]](as trans t)
        case Done() => Done[A, StepListT[G, A]]()
      })(t(step)))

  def dropWhile(p: A => Boolean)(implicit m: Functor[F]): StepListT[F, A] =
    listT[F, A](
      stepMap {
        case Yield(a, as) => if (p(a)) Skip(as dropWhile p) else Yield(a, as)
        case Skip(as) => Skip(as dropWhile p)
        case Done() => Done()
      })

  def takeWhile(p: A => Boolean)(implicit m: Functor[F]): StepListT[F, A] =
    listT[F, A](
      stepMap {
        case Yield(a, as) => if (p(a)) Yield(a, as takeWhile p) else Done()
        case Skip(as) => Skip(as takeWhile p)
        case Done() => Done()
      })

  def ++(bs: => StepListT[F, A])(implicit m: Functor[F]): StepListT[F, A] =
    listT[F, A](
      stepMap {
        case Yield(a, as) => Yield(a, as ++ bs)
        case Skip(as) => Skip(as ++ bs)
        case Done() => Skip(bs)
      })

  def map[B](f: A => B)(implicit m: Functor[F]): StepListT[F, B] =
    listT[F, B](
      stepMap {
        case Yield(a, s) => Yield(f(a), s map f)
        case Skip(s) => Skip(s map f)
        case Done() => Done()
      })

  def flatMap[B](f: A => StepListT[F, B])(implicit m: Functor[F]): StepListT[F, B] =
    listT[F, B](
      stepMap {
        case Yield(a, s) => Skip(f(a) ++ (s flatMap f))
        case Skip(s) => Skip(s flatMap f)
        case Done() => Done()
      })

  def foreach(f: A => F[Unit])(implicit m: Monad[F]): F[Unit] =
    stepFlatMap {
      case Yield(a, s) => m.bd((_: Unit) => s.foreach(f))(f(a))
      case Skip(s) => s.foreach(f)
      case Done() => m.point(())
    }

  private def rev(xs: List[A])(implicit m: Monad[F]): F[List[A]] =
    stepFlatMap {
      case Yield(a, s) => s rev (a :: xs)
      case Skip(s) => s rev xs
      case Done() => m.point(xs)
    }

  def toListT(implicit m: Monad[F]): F[List[A]] =
    m.fmap((_: List[A]).reverse)(rev(Nil))

  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit m: Monad[F]): F[B] =
    m.functor.fmap((_: List[A]).foldLeft(z)((a, b) => f(b, a)))(rev(Nil))

  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit m: Monad[F]): F[B] =
    stepFlatMap {
      case Yield(a, s) => s.foldLeft(f(z, a))(f)
      case Skip(s) => s.foldLeft(z)(f)
      case Done() => m.point(z)
    }

  def length(implicit m: Monad[F]): F[Int] = {
    def addOne(c: => Int, a: => A) = 1 + c
    foldLeft(0)(addOne _)
  }
}

object StepListT extends StepListTs {

  sealed trait Step[A, S]

  case class Yield[A, S](a: A, s: S) extends Step[A, S]

  case class Skip[A, S](s: S) extends Step[A, S]

  case class Done[A, S]() extends Step[A, S]

  protected def listT[F[_], A](s: F[Step[A, StepListT[F, A]]]): StepListT[F, A] = new StepListT[F, A] {
    val step = s
  }

  def apply[F[_], A](implicit p: Pointed[F]): StepListT[F, A] =
    stepListT[F, A]
}

trait StepListTs {
  type StepList[A] = StepListT[Ident, A]

  def stepListT[F[_], A](implicit p: Pointed[F]): StepListT[F, A] = new StepListT[F, A] {
    def step = p.point(StepListT.Done[A, StepListT[F, A]]: StepListT.Step[A, StepListT[F, A]])
  }

  def stepList[A]: StepList[A] =
    stepListT[Ident, A]

  implicit def StepListTFunctor[F[_] : Functor]: Functor[({type λ[X] = StepListT[F, X]})#λ] = new Functor[({type λ[X] = StepListT[F, X]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def StepListTPointed[F[_] : Pointed]
  : Pointed[({type λ[X] = StepListT[F, X]})#λ] = new Pointed[({type λ[X] = StepListT[F, X]})#λ] {
    def point[A](a: => A) =
      a :: stepListT[F, A]
  }

  implicit def StepListTPointedFunctor[F[_]](implicit pf: PointedFunctor[F]): PointedFunctor[({type λ[X] = StepListT[F, X]})#λ] = {
    implicit val p = pf.pointed
    implicit val ftr = pf.functor
    PointedFunctor.pointedFunctor[({type λ[X] = StepListT[F, X]})#λ]
  }

  implicit def StepListTApplic[F[_] : Functor]: Applic[({type λ[X] = StepListT[F, X]})#λ] = new Applic[({type λ[X] = StepListT[F, X]})#λ] {
    def applic[A, B](f: StepListT[F, A => B]) =
      a =>
        for {
          ff <- f
          aa <- a
        } yield ff(aa)
  }

  implicit def StepListTApplicative[F[_]](implicit ap: Applicative[F]): Applicative[({type λ[X] = StepListT[F, X]})#λ] = {
    implicit val p = ap.pointedFunctor
    implicit val ftr = p.functor
    implicit val appl: Applic[F] = ap.applic
    Applicative.applicative[({type λ[X] = StepListT[F, X]})#λ]
  }

  implicit def StepListTBind[F[_] : Functor]: Bind[({type λ[X] = StepListT[F, X]})#λ] = new Bind[({type λ[X] = StepListT[F, X]})#λ] {
    def bind[A, B](f: A => StepListT[F, B]) =
      _ flatMap f
  }

  implicit def StepListTJoin[F[_] : Functor]: Join[({type λ[X] = StepListT[F, X]})#λ] = new Join[({type λ[X] = StepListT[F, X]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def StepListTMonad[F[_]](implicit m: Monad[F]): Monad[({type λ[X] = StepListT[F, X]})#λ] = {
    implicit val ftr = m.functor
    implicit val pt = m.pointed
    Monad.monadBP[({type λ[X] = StepListT[F, X]})#λ]
  }

  implicit def StepListTEmpty[F[_] : Pointed]: Empty[({type λ[X] = StepListT[F, X]})#λ] = new Empty[({type λ[X] = StepListT[F, X]})#λ] {
    def empty[A] =
      stepListT[F, A]
  }

  implicit def StepListTSemigroup[F[_] : Functor, A]: Semigroup[StepListT[F, A]] = new Semigroup[StepListT[F, A]] {
    def append(a1: StepListT[F, A], a2: => StepListT[F, A]) =
      a1 ++ a2
  }

  implicit def StepListTSemigroupZero[F[_] : Pointed, A]: Zero[StepListT[F, A]] = new Zero[StepListT[F, A]] {
    val zero =
      stepListT[F, A]
  }

  implicit def StepListTMonoid[F[_], A](implicit p: PointedFunctor[F]): Monoid[StepListT[F, A]] = {
    implicit val ftr = p.functor
    implicit val pt = p.pointed
    Monoid.monoid
  }

  implicit val StepListTMonadTrans: MonadTrans[StepListT] = new MonadTrans[StepListT] {
    def lift[G[_] : Monad, A](a: G[A]): StepListT[G, A] = new StepListT[G, A] {
      def step = {
        implicit val p = implicitly[Monad[G]].pointed
        implicitly[Monad[G]].fmap((a: A) =>
          StepListT.Yield(a, stepListT[G, A]): StepListT.Step[A, StepListT[G, A]]
        )(a)
      }
    }
  }
}

