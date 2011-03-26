package scalaz

sealed class StreamT[M[_],A](stepper: => M[StreamT.Step[A, StreamT[M,A]]]) {
  def step: M[StreamT.Step[A, StreamT[M,A]]] = stepper

  import StreamT._
  import Scalaz._

  def uncons(implicit M:Monad[M]): M[Option[(A, StreamT[M,A])]] = 
    step flatMap {
      case Yield(a,s) => M.pure(some((a,s)))
      case Skip(s) => s.uncons
      case Done => M.pure(none)
    }
  def ::(a: => A)(implicit M:Pure[M]): StreamT[M,A] = StreamT[M,A](M pure (Yield(a, this)))

  def isEmpty(implicit M:Monad[M]) = uncons map { _.isDefined } 
  def head(implicit M:Monad[M]) = uncons map { _.get._1 }
  def tailM(implicit M:Monad[M]): M[StreamT[M,A]] = uncons map { _.get._2 } 

  def filter(p: A => Boolean)(implicit M:Functor[M]): StreamT[M,A] = StreamT[M,A](
    step map { 
      case Yield(a,as) => if (p(a)) Yield(a, as filter p) else Skip(as filter p)
      case Skip(as) => Skip(as filter p)
      case Done => Done
    }
  )

  def trans[N[_]:Functor](t: M ~> N): StreamT[N,A] = StreamT[N,A](
    t(step) map { 
      case Yield(a,as) => Yield(a, as trans t)
      case Skip(as) => Skip(as trans t)
      case Done => Done
    }
  )

  def dropWhile(p: A => Boolean)(implicit M: Functor[M]): StreamT[M,A] = StreamT[M,A](
    step map { 
      case Yield(a,as) => if (p(a)) Skip(as dropWhile p) else Yield(a,as)
      case Skip(as) => Skip(as dropWhile p)
      case Done => Done
    }
  )
  def takeWhile(p: A => Boolean)(implicit M: Functor[M]): StreamT[M,A] = StreamT[M,A](
    step map { 
      case Yield(a,as) => if (!p(a)) Skip(as takeWhile p) else Yield(a,as)
      case Skip(as) => Skip(as takeWhile p)
      case Done => Done
    }
  )

  def ++[B>:A](bs: => StreamT[M,B])(implicit M:Functor[M]) : StreamT[M,B] = StreamT[M,B](
    step map { 
      case Yield(a,as) => Yield(a, as ++ bs)
      case Skip(as) => Skip(as ++ bs)
      case Done => Skip(bs)
    }
  )
  def flatMap[B](f: A => StreamT[M,B])(implicit M:Functor[M]): StreamT[M,B] = StreamT[M,B](
    step map {
      case Yield(a,s) => Skip(f(a) ++ (s flatMap f))
      case Skip(s) => Skip(s flatMap f)
      case Done => Done
    }
  )
  def map[B](f: A => B)(implicit M:Functor[M]): StreamT[M,B] = StreamT[M,B](
    step map { 
      case Yield(a,s) => Yield(f(a), s map f)
      case Skip(s) => Skip(s map f)
      case Done => Done
    }
  )
  /** Don't use iteratively! */
  def tail(implicit M:Functor[M]): StreamT[M,A] = StreamT[M,A](
    step map { 
      case Yield(a,s) => Skip(s)
      case Skip(s) => Skip(s.tail)
      case Done => sys.error("tail: empty StreamT")
    }
  )
  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Monad[M]): M[B] = 
    step flatMap {
      case Yield(a,s) => s.foldLeft(f(z,a))(f)
      case Skip(s) => s.foldLeft(z)(f)
      case Done => M pure z
    } 

  def toStream(implicit M:Monad[M]): M[Stream[A]] = rev(Stream.Empty) map (_.reverse)

  private def rev(xs : Stream[A])(implicit M:Monad[M]): M[Stream[A]] = 
    step flatMap { 
      case Yield(a,s) => s rev (Stream.cons(a,xs))
      case Skip(s) => s rev xs
      case Done => M pure xs
    }
  
  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Monad[M]): M[B] =
    rev(Stream.Empty) map { _.foldLeft(z)((a, b) => f(b, a)) } 

  def length(implicit M: Monad[M]) : M[Int] = {
    def addOne(c: => Int, a: => A) = 1 + c
    foldLeft(0)(addOne _)
  }

  def foreach(f: A => M[Unit])(implicit M: Monad[M]): M[Unit] = step flatMap {
    case Yield(a,s) => f(a) flatMap (_ => s.foreach(f))
    case Skip(s) => s.foreach(f)
    case Done => M.pure(())
  }
  
}
object StreamT extends Extras { 
  def apply[M[_],A](step: => M[Step[A, StreamT[M,A]]]): StreamT[M,A] = new StreamT[M,A](step)
  def empty[M[_],A](implicit M:Pure[M]): StreamT[M,A] = new StreamT[M,A](M pure Done)

  abstract sealed class Step[+A,+S]
  case class Yield[+A, +S](a: A, s: S) extends Step[A,S]
  case class Skip[+S](s: S) extends Step[Nothing,S]
  case object Done extends Step[Nothing,Nothing]

  implicit def streamTEmpty[M[_]:Pure]
    : Empty[({type λ[X] = StreamT[M,X]})#λ] =
  new Empty[({type λ[X] = StreamT[M,X]})#λ] {
    def empty[A] = StreamT.empty[M,A]
  }
  implicit def streamTFunctor[M[_]:Functor]
    : Functor[({type λ[X] = StreamT[M,X]})#λ] =
  new Functor[({type λ[X] = StreamT[M,X]})#λ] {
    def fmap[A,B](r: StreamT[M, A], f: A => B) = r map f
  }
  implicit def streamTPure[M[_]:Pure]
    : Pure[({type λ[X] = StreamT[M,X]})#λ] =
  new Pure[({type λ[X] = StreamT[M,X]})#λ] {
    def pure[A](a: => A) = a :: empty[M,A]
  }

  implicit def streamTBind[M[_]:Functor]
    : Bind[({type λ[X] = StreamT[M,X]})#λ] =
  new Bind[({type λ[X] = StreamT[M,X]})#λ] {
    def bind[A, B](a: StreamT[M,A], f: A => StreamT[M,B]) = a flatMap f 
  }
  implicit def streamTSemigroup[M[_]:Functor,A]
    : Semigroup[StreamT[M,A]] = 
  new Semigroup[StreamT[M,A]] { 
    def append(s1: StreamT[M,A], s2: => StreamT[M,A]) = s1 ++ s2
  }

  implicit def streamTZero[M[_]:Pure,A]
    : Zero[StreamT[M,A]] =
  new Zero[StreamT[M,A]] {
    val zero = empty[M,A]
  }

  def runStreamT[S,A](stream : StreamT[({type λ[X] = State[S,X]})#λ,A], s0: S)
    : StreamT[Id,A] 
    = StreamT[Id,A](
      stream.step(s0) match { 
        case (s1, Yield(a, as)) => Yield(a, runStreamT(as, s1))
        case (s1, Skip(as)) => Skip(runStreamT(as, s1))
        case (_, Done) => Done
      }
    )
}
