package scalaz

import annotation.tailrec
import Free._
import std.function._
import std.tuple._

// TODO report compiler bug when this appears just above FreeInstances:
//      "java.lang.Error: typeConstructor inapplicable for <none>"
object Free extends FreeFunctions with FreeInstances {

  /** Return from the computation with the given value. */
  case class Return[S[+_]: Functor, +A](a: A) extends Free[S, A]

  /** Suspend the computation with the given suspension. */
  case class Suspend[S[+_]: Functor, +A](a: S[Free[S, A]]) extends Free[S, A]

  /** Call a subroutine and continue with the given function. */
  case class Gosub[S[+_]: Functor, A, +B](a: Free[S, A],
                                 f: A => Free[S, B]) extends Free[S, B]

  /** A computation that can be stepped through, suspended, and paused */
  type Trampoline[+A] = Free[Function0, A]

  /** A computation that produces values of type `A`, eventually resulting in a value of type `B`. */
  type Source[A, +B] = Free[({type f[+x] = (A, x)})#f, B]

  /** A computation that accepts values of type `A`, eventually resulting in a value of type `B`.
    * Note the similarity to an [[scalaz.iteratee.Iteratee]].
    */
  type Sink[A, +B] = Free[({type f[+x] = (=> A) => x})#f, B]
}

/** A free operational monad for some functor `S`. Binding is done using the heap instead of the stack,
  * allowing tail-call elimination. */
sealed abstract class Free[S[+_], +A](implicit S: Functor[S]) {
  final def map[B](f: A => B): Free[S, B] =
    flatMap(a => Return(f(a)))

  /** Alias for `flatMap` */
  final def >>=[B](f: A => Free[S, B]): Free[S, B] = this flatMap f

  /** Binds the given continuation to the result of this computation.
    * All left-associated binds are reassociated to the right. */
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] = this match {
    case Gosub(a, g) => Gosub(a, (x: Any) => Gosub(g(x), f))
    case a           => Gosub(a, f)
  }

  /** Evaluates a single layer of the free monad. */
  @tailrec final def resume: Either[S[Free[S, A]], A] = this match {
    case Return(a)  => Right(a)
    case Suspend(t) => Left(t)
    case a Gosub f  => a match {
      case Return(a)  => f(a).resume
      case Suspend(t) => Left(S.map(t)(((_: Free[S, Any]) flatMap f)))
      case b Gosub g  => b.flatMap((x: Any) => g(x) flatMap f).resume
    }
  }

  /** Changes the suspension functor by the given natural transformation. */
  final def mapSuspension[T[+_]:Functor](f: S ~> T): Free[T, A] =
    resume match {
      case Left(s)  => Suspend(f(S.map(s)(((_: Free[S, A]) mapSuspension f))))
      case Right(r) => Return(r)
    }

  /** Modifies the first suspension with the given natural transformation. */
  final def mapFirstSuspension(f: S ~> S): Free[S, A] = resume match {
    case Left(s) => Suspend(f(s))
    case Right(r) => Return(r)
  }

  /** Applies a function `f` to a value in this monad and a corresponding value in the dual comonad, annihilating both. */
  final def zap[G[+_], B, C](bs: Cofree[G, B])(f: (A, B) => C)(implicit G: Functor[G], d: Duality[S, G]): C =
    Duality.monadComonadDuality.zap(this, bs)(f)

  /** Applies a function in a comonad to the corresponding value in this monad, annihilating both. */
  final def smash[G[+_], B](fs: Cofree[G, A => B])(implicit G: Functor[G], d: Duality[S, G]): B =
    zap(fs)((a, f) => f(a))

  /** Runs a single step, using a function that extracts the resumption from its suspension functor. */
  final def bounce[AA >: A](f: S[Free[S, A]] => Free[S, AA]): Free[S, AA] = resume match {
    case Left(s) => f(s)
    case Right(r) => Return(r)
  }

  /** Runs to completion, using a function that extracts the resumption from its suspension functor. */
  final def go[AA >: A](f: S[Free[S, AA]] => Free[S, AA]): AA = {
    @tailrec def go2(t: Free[S, AA]): AA = t.resume match {
      case Left(s) => go2(f(s))
      case Right(r) => r
    }
    go2(this)
  }

  /** Runs to completion, allowing the resumption function to thread an arbitrary state of type `B`. */
  final def foldRun[B, AA >: A](b: B)(f: (B, S[Free[S, AA]]) => (B, Free[S, AA])): (B, AA) = {
    @tailrec def foldRun2(t: Free[S, AA], z: B): (B, AA) = t.resume match {
      case Left(s) => {
        val (b1, s1) = f(z, s)
        foldRun2(s1, b1) 
      }
      case Right(r) => (z, r)
    }
    foldRun2(this, b)
  }

  import Liskov._

  /** Runs a trampoline all the way to the end, tail-recursively. */
  def run[B >: A](implicit ev: Free[S, B] <~< Trampoline[B]): B =
    ev(this).go(_())

  /** Interleave this computation with another, combining the results with the given function. */
  def zipWith[B, C](tb: Free[S, B], f: (A, B) => C): Free[S, C] = {
    (resume, tb.resume) match {
      case (Left(a), Left(b))   => Suspend(S.map(a)(x => Suspend(S.map(b)(y => x zipWith(y, f)))))
      case (Left(a), Right(b))  => Suspend(S.map(a)(x => x zipWith(Return(b), f)))
      case (Right(a), Left(b))  => Suspend(S.map(b)(y => Return(a)(S) zipWith(y, f)))
      case (Right(a), Right(b)) => Return(f(a, b))
    }
  }

  /** Runs a `Source` all the way to the end, tail-recursively, collecting the produced values. */
  def collect[B, C >: A](implicit ev: Free[S, C] <~< Source[B, C]): (Vector[B], C) = {
    @tailrec def go(c: Source[B, C], v: Vector[B] = Vector()): (Vector[B], C) =
      c.resume match {
        case Left((b, cont)) => go(cont, v :+ b)
        case Right(r)        => (v, r)
      }
    go(ev(this))
  }

  /** Drive this `Source` with the given Sink. */
  def drive[E, B, C >: A](sink: Sink[Option[E], B])(implicit ev: Free[S, C] <~< Source[E, C]): (C, B) = {
    @tailrec def go(src: Source[E, C], snk: Sink[Option[E], B]): (C, B) =
      (src.resume, snk.resume) match {
        case (Left((e, c)), Left(f))  => go(c, f(Some(e)))
        case (Left((e, c)), Right(y)) => go(c, Sink.sinkMonad[Option[E]].pure(y))
        case (Right(x), Left(f))      => go(Source.sourceMonad[E].pure(x), f(None))
        case (Right(x), Right(y))     => (x, y)
      }
    go(ev(this), sink)
  }

  /** Feed the given stream to this `Source`. */
  def feed[E, C >: A](ss: Stream[E])(implicit ev: Free[S, C] <~< Sink[E, C]): C = {
    @tailrec def go(snk: Sink[E, C], rest: Stream[E]): C = (rest, snk.resume) match {
      case (x #:: xs, Left(f)) => go(f(x), xs)
      case (Stream(), Left(f)) => go(f(sys.error("No more values.")), Stream())
      case (_, Right(r))       => r
    }
    go(ev(this), ss)
  }

  /** Feed the given source to this `Sink`. */
  def drain[E, B, C >: A](source: Source[E, B])(implicit ev: Free[S, C] <~< Sink[E, C]): (C, B) = {
    @tailrec def go(src: Source[E, B], snk: Sink[E, C]): (C, B) = (src.resume, snk.resume) match {
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
  implicit val trampolineMonad: Monad[Trampoline] with Copointed[Trampoline] = new Monad[Trampoline] with Copointed[Trampoline] {
    override def point[A](a: => A) = return_[Function0, A](a)
    def bind[A, B](ta: Trampoline[A])(f: A => Trampoline[B]) = ta flatMap f
    def copoint[A](p: Free.Trampoline[A]): A = {
      import std.function.function0Instance
      p.run
    }
  }
}

object Sink extends SinkInstances

trait SinkInstances {
  implicit def sinkMonad[S]: Monad[({type f[x] = Sink[S, x]})#f] =
    new Monad[({type f[x] = Sink[S, x]})#f] {
      def point[A](a: => A) =
        Suspend[({type f[+x] = (=> S) => x})#f, A](s =>
          Return[({type f[+x] = (=> S) => x})#f, A](a))
      def bind[A, B](s: Sink[S, A])(f: A => Sink[S, B]) = s flatMap f
    }
}

object Source extends SourceInstances

trait SourceInstances {
  implicit def sourceMonad[S]: Monad[({type f[x] = Source[S, x]})#f] =
    new Monad[({type f[x] = Source[S, x]})#f] {
      override def point[A](a: => A) = Return[({type f[+x] = (S, x)})#f, A](a)
      def bind[A, B](s: Source[S, A])(f: A => Source[S, B]) = s flatMap f
    }
}

// Trampoline, Sink, and Source are type aliases. We need to add their type class instances
// to Free to be part of the implicit scope.
trait FreeInstances extends TrampolineInstances with SinkInstances with SourceInstances {
  implicit def freeMonad[S[+_]:Functor]: Monad[({type f[x] = Free[S, x]})#f] =
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
  def return_[S[+_], A](value: => A)(implicit S: Pointed[S]): Free[S, A] =
    Suspend[S, A](S.point(Return[S, A](value)))

  def suspend[S[+_], A](value: => Free[S, A])(implicit S: Pointed[S]): Free[S, A] =
    Suspend[S, A](S.point(value))

  /** A trampoline step that doesn't do anything. */
  def pause: Trampoline[Unit] =
    return_(())

  /** A source that produces the given value. */
  def produce[A](a: A): Source[A, Unit] =
    Suspend[({type f[+x] = (A, x)})#f, Unit](a -> Return[({type f[+x] = (A, x)})#f, Unit](()))

  /** A sink that waits for a single value and returns it. */
  def await[A]: Sink[A, A] =
    Suspend[({type f[+x] = (=> A) => x})#f, A](a => Return[({type f[+x] = (=> A) => x})#f, A](a))
}

