package scalaz

sealed trait StepStreamT[F[_], A] {

  import StepStreamT._
  import StateT._
  import Identity._

  protected def step: F[Step[A, StepStreamT[F, A]]]

  def *->* : (({type λ[α] = StepStreamT[F, α]})#λ *->* A) =
    scalaz.*->*.!**->**![({type λ[α] = StepStreamT[F, α]})#λ, A](this)

  private def stepFlatMap[B](k: Step[A, StepStreamT[F, A]] => F[B])(implicit m: Monad[F]): F[B] =
    m.bd(k)(step)

  private def stepMap[B](k: Step[A, StepStreamT[F, A]] => B)(implicit m: Functor[F]): F[B] =
    m.fmap(k)(step)

  def runStream[S](s: S)(implicit i: F[Step[A, StepStreamT[F, A]]] =:= PartialApplyState[S]#Apply[Step[A, StepStreamT[PartialApplyState[S]#Apply, A]]]): StepStream[A] =
    streamT(id(step.run(s) match {
      case (Yield(a, as), s1) => Yield[A, StepStream[A]](a, as runStream s1)
      case (Skip(as), s1) => Skip(as runStream s1)
      case (Done(), _) => Done()
    }))

  def uncons(implicit m: Monad[F]): F[Option[(A, StepStreamT[F, A])]] =
    stepFlatMap {
      case Yield(a, s) => m.point(Some((a, s)))
      case Skip(s) => s.uncons
      case Done() => m.point(None)
    }

  def ::(a: => A)(implicit m: Pointed[F]): StepStreamT[F, A] =
    streamT[F, A](m.point(Yield(a, this)))

  def isEmpty(implicit m: Monad[F]): F[Boolean] =
    m.fmap((_: Option[(A, StepStreamT[F, A])]).isDefined)(uncons)

  def head(implicit m: Monad[F]): F[A] =
    m.fmap((_: Option[(A, StepStreamT[F, A])]).getOrElse(sys.error("head: empty StepStreamT"))._1)(uncons)

  /**Don't use iteratively! */
  def tail(implicit m: Functor[F]): StepStreamT[F, A] =
    streamT[F, A](
      stepMap {
        case Yield(a, s) => Skip(s)
        case Skip(s) => Skip(s.tail)
        case Done() => sys.error("tail: empty StepStreamT")
      })

  def tailM(implicit m: Monad[F]): F[StepStreamT[F, A]] =
    m.fmap((_: Option[(A, StepStreamT[F, A])]).getOrElse(sys.error("tailM: empty StepStreamT"))._2)(uncons)

  def filter(p: A => Boolean)(implicit m: Functor[F]): StepStreamT[F, A] =
    streamT[F, A](
      stepMap {
        case Yield(a, as) => if (p(a)) Yield(a, as filter p) else Skip(as filter p)
        case Skip(as) => Skip(as filter p)
        case Done() => Done()
      })

  def trans[G[_]](t: F ~> G)(implicit m: Functor[G]): StepStreamT[G, A] =
    streamT[G, A](
      m.fmap((x: Step[A, StepStreamT[F, A]]) => x match {
        case Yield(a, as) => Yield[A, StepStreamT[G, A]](a, as trans t): Step[A, StepStreamT[G, A]]
        case Skip(as) => Skip[A, StepStreamT[G, A]](as trans t)
        case Done() => Done[A, StepStreamT[G, A]]()
      })(t(step)))

  def dropWhile(p: A => Boolean)(implicit m: Functor[F]): StepStreamT[F, A] =
    streamT[F, A](
      stepMap {
        case Yield(a, as) => if (p(a)) Skip(as dropWhile p) else Yield(a, as)
        case Skip(as) => Skip(as dropWhile p)
        case Done() => Done()
      })

  def takeWhile(p: A => Boolean)(implicit m: Functor[F]): StepStreamT[F, A] =
    streamT[F, A](
      stepMap {
        case Yield(a, as) => if (p(a)) Yield(a, as takeWhile p) else Done()
        case Skip(as) => Skip(as takeWhile p)
        case Done() => Done()
      })

  def ++(bs: => StepStreamT[F, A])(implicit m: Functor[F]): StepStreamT[F, A] =
    streamT[F, A](
      stepMap {
        case Yield(a, as) => Yield(a, as ++ bs)
        case Skip(as) => Skip(as ++ bs)
        case Done() => Skip(bs)
      })

  def map[B](f: A => B)(implicit m: Functor[F]): StepStreamT[F, B] =
    streamT[F, B](
      stepMap {
        case Yield(a, s) => Yield(f(a), s map f)
        case Skip(s) => Skip(s map f)
        case Done() => Done()
      })

  def flatMap[B](f: A => StepStreamT[F, B])(implicit m: Functor[F]): StepStreamT[F, B] =
    streamT[F, B](
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

  private def rev(xs: Stream[A])(implicit m: Monad[F]): F[Stream[A]] =
    stepFlatMap {
      case Yield(a, s) => s rev (a #:: xs)
      case Skip(s) => s rev xs
      case Done() => m.point(xs)
    }

  def toStreamT(implicit m: Monad[F]): F[Stream[A]] =
    m.fmap((_: Stream[A]).reverse)(rev(Stream.Empty))

  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit m: Monad[F]): F[B] =
    m.fmap((_: Stream[A]).foldLeft(z)((a, b) => f(b, a)))(rev(Stream.Empty))

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

object StepStreamT extends StepStreamTs {

  sealed trait Step[A, S]

  case class Yield[A, S](a: A, s: S) extends Step[A, S]

  case class Skip[A, S](s: S) extends Step[A, S]

  case class Done[A, S]() extends Step[A, S]

  protected def streamT[F[_], A](s: F[Step[A, StepStreamT[F, A]]]): StepStreamT[F, A] = new StepStreamT[F, A] {
    val step = s
  }

  def apply[F[_], A](implicit p: Pointed[F]): StepStreamT[F, A] =
    stepStreamT[F, A]
}

trait StepStreamTs {
  type StepStream[A] = StepStreamT[Identity, A]

  def stepStreamT[F[_], A](implicit p: Pointed[F]): StepStreamT[F, A] = new StepStreamT[F, A] {
    def step = p.point(StepStreamT.Done[A, StepStreamT[F, A]]: StepStreamT.Step[A, StepStreamT[F, A]])
  }

  def stepStream[A]: StepStream[A] =
    stepStreamT[Identity, A]

  implicit def StepStreamTFunctor[F[_] : Functor]: Functor[({type λ[X] = StepStreamT[F, X]})#λ] = new Functor[({type λ[X] = StepStreamT[F, X]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def StepStreamTPointed[F[_] : Pointed]
  : Pointed[({type λ[X] = StepStreamT[F, X]})#λ] = new Pointed[({type λ[X] = StepStreamT[F, X]})#λ] {
    def point[A](a: => A) =
      a :: stepStreamT[F, A]
  }

  implicit def StepStreamTPointedFunctor[F[_]](implicit pf: PointedFunctor[F]): PointedFunctor[({type λ[X] = StepStreamT[F, X]})#λ] = {
    implicit val p = pf.pointed
    implicit val ftr = pf.functor
    PointedFunctor.pointedFunctor[({type λ[X] = StepStreamT[F, X]})#λ]
  }

  implicit def StepStreamTApplic[F[_] : Functor]: Applic[({type λ[X] = StepStreamT[F, X]})#λ] = new Applic[({type λ[X] = StepStreamT[F, X]})#λ] {
    def applic[A, B](f: StepStreamT[F, A => B]) =
      a =>
        for {
          ff <- f
          aa <- a
        } yield ff(aa)
  }

  implicit def StepStreamTApplicative[F[_]](implicit ap: Applicative[F]): Applicative[({type λ[X] = StepStreamT[F, X]})#λ] = {
    implicit val p = ap.pointedFunctor
    implicit val ftr = p.functor
    implicit val appl: Applic[F] = ap.applic
    Applicative.applicative[({type λ[X] = StepStreamT[F, X]})#λ]
  }

  implicit def StepStreamTBind[F[_] : Functor]: Bind[({type λ[X] = StepStreamT[F, X]})#λ] = new Bind[({type λ[X] = StepStreamT[F, X]})#λ] {
    def bind[A, B](f: A => StepStreamT[F, B]) =
      _ flatMap f
  }

  implicit def StepStreamTJoin[F[_] : Functor]: Join[({type λ[X] = StepStreamT[F, X]})#λ] = new Join[({type λ[X] = StepStreamT[F, X]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def StepStreamTMonad[F[_]](implicit m: Monad[F]): Monad[({type λ[X] = StepStreamT[F, X]})#λ] = {
    implicit val ftr = m.functor
    implicit val pt = m.pointed
    Monad.monadBP[({type λ[X] = StepStreamT[F, X]})#λ]
  }

  implicit def StepStreamTEmpty[F[_] : Pointed]: Empty[({type λ[X] = StepStreamT[F, X]})#λ] = new Empty[({type λ[X] = StepStreamT[F, X]})#λ] {
    def empty[A] =
      stepStreamT[F, A]
  }

  implicit def StepStreamTSemigroup[F[_] : Functor, A]: Semigroup[StepStreamT[F, A]] = new Semigroup[StepStreamT[F, A]] {
    def append(a1: StepStreamT[F, A], a2: => StepStreamT[F, A]) =
      a1 ++ a2
  }

  implicit def StepStreamTSemigroupZero[F[_] : Pointed, A]: Zero[StepStreamT[F, A]] = new Zero[StepStreamT[F, A]] {
    val zero =
      stepStreamT[F, A]
  }

  implicit def StepStreamTMonoid[F[_], A](implicit p: PointedFunctor[F]): Monoid[StepStreamT[F, A]] = {
    implicit val ftr = p.functor
    implicit val pt = p.pointed
    Monoid.monoid
  }

  implicit val StepStreamTMonadTrans: MonadTrans[StepStreamT] = new MonadTrans[StepStreamT] {
    def lift[G[_] : Monad, A](a: G[A]): StepStreamT[G, A] = new StepStreamT[G, A] {
      def step = {
        implicit val p = implicitly[Monad[G]].pointed
        implicitly[Monad[G]].fmap((a: A) =>
          StepStreamT.Yield(a, stepStreamT[G, A]): StepStreamT.Step[A, StepStreamT[G, A]]
        )(a)
      }
    }
  }

}