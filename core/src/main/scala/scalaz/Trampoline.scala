package scalaz

import annotation.tailrec
import Trampoline._
import std.function._
import std.tuple._

object Trampoline extends TrampolineFunctions with TrampolineInstances {

  /** Return from the computation with the given value. */
  case class Return[+A](a: A) extends Trampoline[A]

  /** Bounce on the trampoline and wait to be called. */
  case class Bounce[+A](a: S[Trampoline[A]]) extends Trampoline[A]

  /** Call a subroutine and continue with the given function. */
  case class Gosub[A, +B](a: Trampoline[A],
                          f: A => Trampoline[B]) extends Trampoline[B]
}

/** Trampoline is a specialized free monad that allows structuring of tail-recursive computations that
  * can be suspended and resumed. Method calls, even calls not in tail position, are eliminated.
  * This trampoline will unwind the stack every 1000 calls. */
sealed abstract class Trampoline[+A] {
  final def map[B](f: A => B): Trampoline[B] =
    flatMap(a => Return(f(a)))

  /** Alias for `flatMap` */
  final def >>=[B](f: A => Trampoline[B]): Trampoline[B] = this flatMap f

  /** Binds the given continuation to the result of this computation.
    * All left-associated binds are reassociated to the right. */
  final def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = this match {
    case Gosub(a, g) => Gosub(a, (x: Any) => Gosub(g(x), f))
    case a           => Gosub(a, f)
  }

  /** Runs a single step. */
  @tailrec final def resume: Either[() => Trampoline[A], A] = this match {
    case Return(a)  => Right(a)
    case Bounce(t) => Left(t)
    case a Gosub f  => a match {
      case Return(a)  => f(a).resume
      case Bounce(t) => Left(S.map(t)(((_: Trampoline[Any]) flatMap f)))
      case b Gosub g  => b.flatMap((x: Any) => g(x) flatMap f).resume
    }
  }

  /** Runs a trampoline all the way to the end, tail-recursively. */
  def run: A = resume match {
    case Left(k) => k().run
    case Right(a) => a
  }

  /** Interleave this computation with another, combining the results with the given function. */
  def zipWith[B, C](tb: Trampoline[B], f: (A, B) => C): Trampoline[C] = {
    (resume, tb.resume) match {
      case (Left(a), Left(b))   => Bounce(a.map(x => Bounce(b.map(y => x zipWith(y, f)))))
      case (Left(a), Right(b))  => Bounce(a.map(x => x zipWith(Return(b), f)))
      case (Right(a), Left(b))  => Bounce(b.map(y => Return(a) zipWith(y, f)))
      case (Right(a), Right(b)) => Return(f(a, b))
    }
  }

  /** Collapse a Trampoline to a single step. */
  def reset: Trampoline[A] = { val a = run; Return(a) }
}

object Trampoline extends TrampolineInstances

trait TrampolineInstances {
  implicit val trampolineMonad: Monad[Trampoline] with CoPointed[Trampoline] = new Monad[Trampoline] with CoPointed[Trampoline] {
    override def point[A](a: => A) = done[Function0, A](a)
    def bind[A, B](ta: Trampoline[A])(f: A => Trampoline[B]) = ta flatMap f
    def copoint[A](p: Trampoline.Trampoline[A]): A = {
      import std.function.function0Instance
      p.run
    }
  }
}

object Sink extends SinkInstances

trait SinkInstances {
  implicit def sinkMonad[S]: Monad[({type f[x] = Sink[x]})#f] =
    new Monad[({type f[x] = Sink[x]})#f] {
      def point[A](a: => A) =
        Bounce[({type f[+x] = (=> S) => x})#f, A](s =>
          Return[({type f[+x] = (=> S) => x})#f, A](a))
      def bind[A, B](s: Sink[A])(f: A => Sink[B]) = s flatMap f
    }
}

object Source extends SourceInstances

trait SourceInstances {
  implicit def sourceMonad[S]: Monad[({type f[x] = Source[x]})#f] =
    new Monad[({type f[x] = Source[x]})#f] {
      override def point[A](a: => A) = Return[({type f[+x] = (x)})#f, A](a)
      def bind[A, B](s: Source[A])(f: A => Source[B]) = s flatMap f
    }
}

// Trampoline, Sink, and Source are type aliases. We need to add their type class instances
// to Trampoline to be part of the implicit scope.
trait TrampolineInstances extends TrampolineInstances with SinkInstances with SourceInstances {
  implicit def freeMonad[S[+_]:Functor]: Monad[({type f[x] = Trampoline[x]})#f] =
    new Monad[({type f[x] = Trampoline[x]})#f] {
      def point[A](a: => A) = Return(a)
      override def map[A, B](fa: Trampoline[A])(f: A => B) = fa map f
      def bind[A, B](a: Trampoline[A])(f: A => Trampoline[B]) = a flatMap f
    }
}

trait TrampolineFunctions {

  /** Suspend the given computation in a single step. */
  def return_[A](value: => A)(implicit S: Pointed[S]): Trampoline[A] =
    Bounce[A](S.point(Return[A](value)))

  def suspend[A](value: => Trampoline[A])(implicit S: Pointed[S]): Trampoline[A] =
    Bounce[A](S.point(value))

  /** A trampoline step that doesn't do anything. */
  def pause: Trampoline[Unit] =
    return_(())

  /** A source that produces the given value. */
  def produce[A](a: A): Source[A, Unit] =
    Bounce[({type f[+x] = (A, x)})#f, Unit](a -> Return[({type f[+x] = (A, x)})#f, Unit](()))

  /** A sink that waits for a single value and returns it. */
  def await[A]: Sink[A, A] =
    Bounce[({type f[+x] = (=> A) => x})#f, A](a => Return[({type f[+x] = (=> A) => x})#f, A](a))
}


