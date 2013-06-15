package scalaz

import annotation.tailrec
import Free._
import std.function._
import std.tuple._

// TODO report compiler bug when this appears just above FreeInstances:
//      "java.lang.Error: typeConstructor inapplicable for <none>"
object Free extends FreeFunctions with FreeInstances {

  /** Return from the computation with the given value. */
  case class Return[S[_]: Functor, A](a: A) extends Free[S, A]

  /** Suspend the computation with the given suspension. */
  case class Suspend[S[_]: Functor, A](a: S[Free[S, A]]) extends Free[S, A]

  /** Call a subroutine and continue with the given function. */
  case class Gosub[S[_]: Functor, A, B](a: () => Free[S, A],
                                        f: A => Free[S, B]) extends Free[S, B]

  /** A computation that can be stepped through, suspended, and paused */
  type Trampoline[A] = Free[Function0, A]

  /** A computation that produces values of type `A`, eventually resulting in a value of type `B`. */
  type Source[A, B] = Free[({type f[x] = (A, x)})#f, B]

  /** A computation that accepts values of type `A`, eventually resulting in a value of type `B`.
    * Note the similarity to an [[scalaz.iteratee.Iteratee]].
    */
  type Sink[A, B] = Free[({type f[x] = (=> A) => x})#f, B]
}

/** A free operational monad for some functor `S`. Binding is done using the heap instead of the stack,
  * allowing tail-call elimination. */
sealed abstract class Free[S[_], A](implicit S: Functor[S]) {
  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Return(f(a)))

  /** Alias for `flatMap` */
  final def >>=[B](f: A => Free[S, B]): Free[S, B] = this flatMap f

  /** Binds the given continuation to the result of this computation.
    * All left-associated binds are reassociated to the right. */
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] = this match {
    case Gosub(a, g) => Gosub(a, (x: Any) => Gosub(() => g(x), f))
    case a           => Gosub(() => a, f)
  }

  /** Evaluates a single layer of the free monad. */
  @tailrec final def resume: (S[Free[S, A]] \/ A) = this match {
    case Return(a)  => \/.right(a)
    case Suspend(t) => \/.left(t)
    case a Gosub f  => a() match {
      case Return(a)  => f(a).resume
      case Suspend(t) => \/.left(S.map(t)(((_: Free[S, Any]) flatMap f)))
      case b Gosub g  => b().flatMap((x: Any) => g(x) flatMap f).resume
    }
  }

  /** Changes the suspension functor by the given natural transformation. */
  final def mapSuspension[T[_]:Functor](f: S ~> T): Free[T, A] =
    resume match {
      case -\/(s)  => Suspend(f(S.map(s)(((_: Free[S, A]) mapSuspension f))))
      case \/-(r) => Return(r)
    }

  /** Modifies the first suspension with the given natural transformation. */
  final def mapFirstSuspension(f: S ~> S): Free[S, A] = resume match {
    case -\/(s) => Suspend(f(s))
    case \/-(r) => Return(r)
  }

  /** Applies a function `f` to a value in this monad and a corresponding value in the dual comonad, annihilating both. */
  final def zapWith[G[_], B, C](bs: Cofree[G, B])(f: (A, B) => C)(implicit G: Functor[G], d: Zap[S, G]): C =
    Zap.monadComonadZap.zapWith(this, bs)(f)

  /** Applies a function in a comonad to the corresponding value in this monad, annihilating both. */
  final def zap[G[_], B](fs: Cofree[G, A => B])(implicit G: Functor[G], d: Zap[S, G]): B =
    zapWith(fs)((a, f) => f(a))

  /** Runs a single step, using a function that extracts the resumption from its suspension functor. */
  final def bounce(f: S[Free[S, A]] => Free[S, A]): Free[S, A] = resume match {
    case -\/(s) => f(s)
    case \/-(r) => Return(r)
  }

  /** Runs to completion, using a function that extracts the resumption from its suspension functor. */
  final def go(f: S[Free[S, A]] => Free[S, A]): A = {
    @tailrec def go2(t: Free[S, A]): A = t.resume match {
      case -\/(s) => go2(f(s))
      case \/-(r) => r
    }
    go2(this)
  }

  final def runM[M[_]:Monad](f: S[Free[S, A]] => M[Free[S, A]]): M[A] = {
    def runM2(t: Free[S, A]): M[A] = t.resume match {
      case -\/(s) => Monad[M].bind(f(s))(runM2)
      case \/-(r) => Monad[M].pure(r)
    }
    runM2(this)
  }

  /** Runs to completion, allowing the resumption function to thread an arbitrary state of type `B`. */
  final def foldRun[B](b: B)(f: (B, S[Free[S, A]]) => (B, Free[S, A])): (B, A) = {
    @tailrec def foldRun2(t: Free[S, A], z: B): (B, A) = t.resume match {
      case -\/(s) =>
        val (b1, s1) = f(z, s)
        foldRun2(s1, b1)
      case \/-(r) => (z, r)
    }
    foldRun2(this, b)
  }

  /** Runs a trampoline all the way to the end, tail-recursively. */
  def run(implicit ev: Free[S, A] =:= Trampoline[A]): A =
    ev(this).go(_())

  /** Interleave this computation with another, combining the results with the given function. */
  def zipWith[B, C](tb: Free[S, B], f: (A, B) => C): Free[S, C] = {
    (resume, tb.resume) match {
      case (-\/(a), -\/(b))   => Suspend(S.map(a)(x => Suspend(S.map(b)(y => x zipWith(y, f)))))
      case (-\/(a), \/-(b))  => Suspend(S.map(a)(x => x zipWith(Return(b), f)))
      case (\/-(a), -\/(b))  => Suspend(S.map(b)(y => Return(a)(S) zipWith(y, f)))
      case (\/-(a), \/-(b)) => Return(f(a, b))
    }
  }

  /** Runs a `Source` all the way to the end, tail-recursively, collecting the produced values. */
  def collect[B](implicit ev: Free[S, A] =:= Source[B, A]): (Vector[B], A) = {
    @tailrec def go(c: Source[B, A], v: Vector[B] = Vector()): (Vector[B], A) =
      c.resume match {
        case -\/((b, cont)) => go(cont, v :+ b)
        case \/-(r)         => (v, r)
      }
    go(ev(this))
  }

  /** Drive this `Source` with the given Sink. */
  def drive[E, B](sink: Sink[Option[E], B])(implicit ev: Free[S, A] =:= Source[E, A]): (A, B) = {
    @tailrec def go(src: Source[E, A], snk: Sink[Option[E], B]): (A, B) =
      (src.resume, snk.resume) match {
        case (-\/((e, c)), -\/(f)) => go(c, f(Some(e)))
        case (-\/((e, c)), \/-(y)) => go(c, Sink.sinkMonad[Option[E]].pure(y))
        case (\/-(x), -\/(f))      => go(Source.sourceMonad[E].pure(x), f(None))
        case (\/-(x), \/-(y))      => (x, y)
      }
    go(ev(this), sink)
  }

  /** Feed the given stream to this `Source`. */
  def feed[E](ss: Stream[E])(implicit ev: Free[S, A] =:= Sink[E, A]): A = {
    @tailrec def go(snk: Sink[E, A], rest: Stream[E]): A = (rest, snk.resume) match {
      case (x #:: xs, -\/(f)) => go(f(x), xs)
      case (Stream(), -\/(f)) => go(f(sys.error("No more values.")), Stream())
      case (_, \/-(r))        => r
    }
    go(ev(this), ss)
  }

  /** Feed the given source to this `Sink`. */
  def drain[E, B](source: Source[E, B])(implicit ev: Free[S, A] =:= Sink[E, A]): (A, B) = {
    @tailrec def go(src: Source[E, B], snk: Sink[E, A]): (A, B) = (src.resume, snk.resume) match {
      case (-\/((e, c)), -\/(f)) => go(c, f(e))
      case (-\/((e, c)), \/-(y)) => go(c, Sink.sinkMonad[E].pure(y))
      case (\/-(x), -\/(f))      => sys.error("Not enough values in source.")
      case (\/-(x), \/-(y))      => (y, x)
    }
    go(source, ev(this))
  }
}

object Trampoline extends TrampolineInstances {

  def done[A](a: A): Trampoline[A] =
    Free.Return[Function0,A](a)

  def delay[A](a: => A): Trampoline[A] =
    suspend(done(a))

  def suspend[A](a: => Trampoline[A]): Trampoline[A] =
    Free.Suspend[Function0, A](() => a)
}

trait TrampolineInstances {
  implicit val trampolineMonad: Monad[Trampoline] = new Monad[Trampoline] {
    override def point[A](a: => A) = return_[Function0, A](a)
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

// Trampoline, Sink, and Source are type aliases. We need to add their type class instances
// to Free to be part of the implicit scope.
trait FreeInstances extends TrampolineInstances with SinkInstances with SourceInstances {
  implicit def freeMonad[S[_]:Functor]: Monad[({type f[x] = Free[S, x]})#f] =
    new Monad[({type f[x] = Free[S, x]})#f] {
      def point[A](a: => A) = Return(a)
      override def map[A, B](fa: Free[S, A])(f: A => B) = fa map f
      def bind[A, B](a: Free[S, A])(f: A => Free[S, B]) = a flatMap f
    }
}

trait FreeFunctions {
  /** Collapse a trampoline to a single step. */
  def reset[A](r: Trampoline[A]): Trampoline[A] = { val a = r.run; return_(a) }

  /** Suspend the given computation in a single step. */
  def return_[S[_], A](value: => A)(implicit S: Applicative[S]): Free[S, A] =
    Suspend[S, A](S.point(Return[S, A](value)))

  def suspend[S[_], A](value: => Free[S, A])(implicit S: Applicative[S]): Free[S, A] =
    Suspend[S, A](S.point(value))

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

