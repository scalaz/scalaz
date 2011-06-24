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

  def liftStepStreamT[G[_] : Monad, A](a: G[A]): StepStreamT[G, A] = new StepStreamT[G, A] {
    def step = {
      implicit val p = implicitly[Monad[G]].pointed
      implicitly[Monad[G]].fmap((a: A) =>
        StepStreamT.Yield(a, StepStreamT.stepStreamT[G, A]): StepStreamT.Step[A, StepStreamT[G, A]]
      )(a)
    }
  }

}