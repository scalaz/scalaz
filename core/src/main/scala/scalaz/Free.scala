package scalaz

import annotation.tailrec
import Free._
import std.function._
import std.tuple._

// TODO report compiler bug when this appears just above FreeInstances:
//      "java.lang.Error: typeConstructor inapplicable for <none>"
object Free extends FreeFunctions with FreeInstances {

  /** Return from the computation with the given value. */
  case class Return[S[_], A](a: A) extends Free[S, A]

  /** Suspend the computation with the given suspension. */
  case class Suspend[S[_], A](a: S[Free[S, A]]) extends Free[S, A]

  /** Call a subroutine and continue with the given function. */
  case class Gosub[S[_], A, B](a: Free[S, A],
                               f: A => Free[S, B]) extends Free[S, B]

  /** A computation that can be stepped through, suspended, and paused */
  type Trampoline[A] = Free[Function0, A]

  /** A computation that produces values of type `A`, eventually resulting in a value of type `B`. */
  type Source[A, B] = Free[({type f[x] = (A, x)})#f, B]

  /** A computatio that accepts values of type `A`, eventually resulting in a value of type `B`.
    * Note the similarity to an Iteratee.
    */
  type Sink[A, B] = Free[({type f[x] = (=> A) => x})#f, B]
}

/** A free operational monad for some functor `S`. Binding is done using the heap instead of the stack,
  * allowing tail-call elimination. */
sealed trait Free[S[_], A] {
  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Return(f(a)))

  /** Binds the given continuation to the result of this computation. 
    * All left-associated binds are reassociated to the right. */
  final def >>=[B](f: A => Free[S, B]): Free[S, B] = this match {
    case Gosub(a, g) => Gosub(a, (x: Any) => Gosub(g(x), f))
    case a           => Gosub(a, f)
  }

  /** Binds the given continuation to the result of this computation.
    * All left-associated binds are reassociated to the right. */
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] =
    this >>= f

  /** Evaluates a single layer of the free monad. */
  @tailrec final def resume(implicit fun: Functor[S]): Either[S[Free[S, A]], A] = this match {
    case Return(a)  => Right(a)
    case Suspend(t) => Left(t)
    case a Gosub f  => a match {
      case Return(a)  => f(a).resume
      case Suspend(t) => Left(fun.map(t)(((_: Free[S, Any]) >>= f)))
      case b Gosub g  => (Gosub(b, (x: Any) => Gosub(g(x), f)): Free[S, A]).resume
    }
  }

  /** Modifies the suspension with the given natural transformation. */
  final def mapSuspension[T[_]](f: S ~> T)(implicit fun: Functor[S]): Free[T, A] =
    resume match {
      case Left(s)  => Suspend(f(fun.map(s)(((_: Free[S, A]) mapSuspension f))))
      case Right(r) => Return(r)
    }

  import Liskov._

  /** Runs a trampoline all the way to the end, tail-recursively. */
  def run(implicit ev: Free[S, A] <~< Trampoline[A], fun: Functor[S]): A = {
    @tailrec def go(t: Trampoline[A]): A =
      t.resume match {
        case Left(s)  => go(s())
        case Right(a) => a
      }
    go(ev(this))
  }

  /** Interleave this computation with another, combining the results with the given function. */
  def zipWith[B, C](tb: Free[S, B], f: (A, B) => C)(implicit S: Functor[S]): Free[S, C] = {
    (resume, tb.resume) match {
      case (Left(a), Left(b))   => Suspend(S.map(a)(x => Suspend(S.map(b)(y => x zipWith(y, f)))))
      case (Left(a), Right(b))  => Suspend(S.map(a)(x => x zipWith(Return(b), f)))
      case (Right(a), Left(b))  => Suspend(S.map(b)(y => Return(a) zipWith(y, f)))
      case (Right(a), Right(b)) => Return(f(a, b))
    }
  }

  /** Runs a Source all the way to the end, tail-recursively, collecting the produced values. */
  def collect[B](implicit ev: Free[S, A] <~< Source[B, A],
                 fun: Functor[S]): (Vector[B], A) = {
    @tailrec def go(c: Source[B, A], v: Vector[B] = Vector()): (Vector[B], A) =
      c.resume match {
        case Left((b, cont)) => go(cont, v :+ b)
        case Right(r)        => (v, r)
      }
    go(ev(this))
  }

  /** Drive this Source with the given Sink. */
  def drive[E, B](sink: Sink[Option[E], B])(implicit ev: Free[S, A] <~< Source[E, A], fun: Functor[S]): (A, B) = {
    @tailrec def go(src: Source[E, A], snk: Sink[Option[E], B]): (A, B) =
      (src.resume, snk.resume) match {
        case (Left((e, c)), Left(f))  => go(c, f(Some(e)))
        case (Left((e, c)), Right(y)) => go(c, Sink.sinkMonad[Option[E]].pure(y))
        case (Right(x), Left(f))      => go(Source.sourceMonad[E].pure(x), f(None))
        case (Right(x), Right(y))     => (x, y)
      }
    go(ev(this), sink)
  }

  /** Feed the given stream to this Source. */
  def feed[E](ss: Stream[E])(implicit ev: Free[S, A] <~< Sink[E, A], fun: Functor[S]): A = {
    @tailrec def go(snk: Sink[E, A], rest: Stream[E]): A = (rest, snk.resume) match {
      case (x #:: xs, Left(f)) => go(f(x), xs)
      case (Stream(), Left(f)) => go(f(sys.error("No more values.")), Stream())
      case (_, Right(r))       => r
    }
    go(ev(this), ss)
  }

  /** Feed the given source to this Sink. */
  def drain[E, B](source: Source[E, B])(implicit ev: Free[S, A] <~< Sink[E, A], fun: Functor[S]): (A, B) = {
    @tailrec def go(src: Source[E, B], snk: Sink[E, A]): (A, B) = (src.resume, snk.resume) match {
      case (Left((e, c)), Left(f))  => go(c, f(e))
      case (Left((e, c)), Right(y)) => go(c, Sink.sinkMonad[E].pure(y))
      case (Right(x), Left(f))      => sys.error("Not enough values in source.")
      case (Right(x), Right(y))     => (y, x)
    }
    go(source, ev(this))
  }
}

object Trampoline extends TrampolineInstances

trait TrampolineInstances {
  implicit val trampolineMonad: Monad[Trampoline] = new Monad[Trampoline] {
    override def point[A](a: => A) = return_(a)
    def bind[A, B](ta: Trampoline[A])(f: A => Trampoline[B]) = ta flatMap f
  }
}

object Sink extends SinkInstances

trait SinkInstances {
  implicit def sinkMonad[S]: Monad[({type f[x] = Sink[S, x]})#f] =
    new Monad[({type f[x] = Sink[S, x]})#f] {
      def point[A](a: => A) =
        Suspend[({type f[x] = (=> S) => x})#f, A](s =>
          Return[({type f[x] = (=> S) => x})#f, A](a))
      def bind[A, B](s: Sink[S, A])(f: A => Sink[S, B]) = s flatMap f
    }
}

object Source extends SourceInstances

trait SourceInstances {
  implicit def sourceMonad[S]: Monad[({type f[x] = Source[S, x]})#f] =
    new Monad[({type f[x] = Source[S, x]})#f] {
      override def point[A](a: => A) = Return[({type f[x] = (S, x)})#f, A](a)
      def bind[A, B](s: Source[S, A])(f: A => Source[S, B]) = s flatMap f
    }
}

trait FreeInstances {
  implicit def coroutineMonad[S[_]]: Monad[({type f[x] = Free[S, x]})#f] =
    new Monad[({type f[x] = Free[S, x]})#f] {
      def point[A](a: => A) = Return(a)
      override def map[A, B](fa: Free[S, A])(f: A => B) = fa map f
      def bind[A, B](a: Free[S, A])(f: A => Free[S, B]) = a flatMap f
    }
}

trait FreeFunctions {
  /** Collapse a trampoline to a single step. */
  def reset[A](r: Trampoline[A]): Trampoline[A] = return_(r.run)

  /** Suspend the given computation in a single step. */
  def return_[S[_], A](value: => A)(implicit p: Pointed[S]): Free[S, A] =
    Suspend[S, A](p.point(Return[S, A](value)))

  def suspend[S[_], A](value: => Free[S, A])(implicit p: Pointed[S]): Free[S, A] =
    Suspend[S, A](p.point(value))

  /** A trampoline step that doesn't do anything. */
  def pause: Trampoline[Unit] =
    return_(())

  /** A source that produces the given value. */
  def produce[A](a: A): Source[A, Unit] =
    Suspend[({type f[x] = (A, x)})#f, Unit](a -> Return[({type f[x] = (A, x)})#f, Unit](()))

  /** A sink that waits for a single value and returns it. */
  def await[A]: Sink[A, A] =
    Suspend[({type f[x] = (=> A) => x})#f, A](a => Return[({type f[x] = (=> A) => x})#f, A](a))
}

