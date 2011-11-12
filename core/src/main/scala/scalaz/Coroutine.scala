package scalaz

import annotation.tailrec
import Coroutine._
import std.function._
import std.tuple._

case class Return[S[_], A](a: A) extends Coroutine[S, A]
case class Suspend[S[_], A](a: S[Coroutine[S, A]]) extends Coroutine[S, A]
case class Gosub[S[_], A, B](a: Coroutine[S, A],
                             f: A => Coroutine[S, B]) extends Coroutine[S, B]

case class Control[S[_], A](a: Coroutine[S, A]) extends Throwable

sealed trait Coroutine[S[_], A] {
  final def map[B](f: A => B): Coroutine[S, B] =
    flatMap(a => Return(f(a)))
  def >>=[B](f: A => Coroutine[S, B]): Coroutine[S, B] = this match {
    case Gosub(a, g) => Gosub(a, (x: Any) => Gosub(g(x), f))
    case a => Gosub(a, f)
  }
  final def flatMap[B](f: A => Coroutine[S, B]): Coroutine[S, B] =
    this >>= f
  @tailrec final def resume(implicit fun: Functor[S]): Either[S[Coroutine[S, A]], A] = this match {
    case Return(a) => Right(a)
    case Suspend(t) => Left(t)
    case a Gosub f => a match {
      case Return(a) => f(a).resume
      case Suspend(t) => Left(fun.map(t)(((_:Coroutine[S, Any]) >>= f)))
      case b Gosub g => (Gosub(b, (x: Any) => Gosub(g(x), f)):Coroutine[S, A]).resume
    }
  }
  final def mapSuspension[T[_]](f: S ~> T)(implicit fun: Functor[S]): Coroutine[T, A] = 
    resume match {
      case Left(s) => Suspend(f(fun.map(s)(((_:Coroutine[S, A]) mapSuspension f))))
      case Right(r) => Return(r)
    }
  import Liskov._
  def run(implicit ev: Coroutine[S, A] <~< Trampoline[A], fun: Functor[S]): A = {
    @tailrec def go(t: Trampoline[A]): A = 
      t.resume match {
        case Left(s) => go(s())
        case Right(a) => a
      }
    go(ev(this))
  }
  def zipWith[B, C](tb: Coroutine[S, B], f: (A, B) => C)(implicit fun: Functor[S]): Coroutine[S, C] = {
    (resume, tb.resume) match {
      case (Left(a), Left(b)) => Suspend(fun.map(a)(x => Suspend(fun.map(b)(y => x zipWith (y, f)))))
      case (Left(a), Right(b)) => Suspend(fun.map(a)(x => x zipWith (Return(b), f)))
      case (Right(a), Left(b)) => Suspend(fun.map(b)(y => Return(a) zipWith (y, f)))
      case (Right(a), Right(b)) => Return(f(a, b))
    }
  }
  def collect[B](implicit ev: Coroutine[S, A] <~< Source[B, A],
                          fun: Functor[S]): (Vector[B], A) = {
    @tailrec def go(c: Source[B, A], v: Vector[B] = Vector()): (Vector[B], A) =
      c.resume match {
        case Left((b, cont)) => go(cont, v :+ b)
        case Right(r) => (v, r)
      }
    go(ev(this))
  }
  def drive[E, B](sink: Sink[Option[E], B])(implicit ev: Coroutine[S, A] <~<  Source[E, A], fun: Functor[S]): (A, B) = {
    @tailrec def go(src: Source[E, A], snk: Sink[Option[E], B]): (A, B) =
      (src.resume, snk.resume) match {
        case (Left((e, c)), Left(f)) => go(c, f(Some(e)))
        case (Left((e, c)), Right(y)) => go(c, Sink.sinkMonad[Option[E]].pure(y))
        case (Right(x), Left(f)) => go(Source.sourceMonad[E].pure(x), f(None))
        case (Right(x), Right(y)) => (x, y)
      }
    go(ev(this), sink)
  }

  def feed[E](ss: Stream[E])(implicit ev: Coroutine[S, A] <~< Sink[E, A], fun: Functor[S]): A = {
    @tailrec def go(snk: Sink[E, A], rest: Stream[E]): A = (rest, snk.resume) match {
      case (x #:: xs, Left(f)) => go(f(x), xs)
      case (Stream(), Left(f)) => go(f(sys.error("No more values.")), Stream())
      case (_, Right(r)) => r
    }
    go(ev(this), ss)
  }

  def drain[E, B](source: Source[E, B])(implicit ev: Coroutine[S, A] <~< Sink[E, A], fun: Functor[S]): (A, B) = {
    @tailrec def go(src: Source[E, B], snk: Sink[E, A]): (A, B) = (src.resume, snk.resume) match {
      case (Left((e, c)), Left(f)) => go(c, f(e))
      case (Left((e, c)), Right(y)) => go(c, Sink.sinkMonad[E].pure(y))
      case (Right(x), Left(f)) => sys.error("Not enough values in source.")
      case (Right(x), Right(y)) => (y, x)
    }
    go(source, ev(this))
  }
}

case class Request[I, O, A](request: I, response: O => A)

object Trampoline {
  implicit val trampolineMonad: Monad[Trampoline] = new Monad[Trampoline] {
    override def point[A](a: => A) = suspend(a)
    def bind[A, B](ta: Trampoline[A])(f: A => Trampoline[B]) = ta flatMap f
  }
}

object Sink {
  implicit def sinkMonad[S]: Monad[({type f[x] = Sink[S, x]})#f] =
    new Monad[({type f[x] = Sink[S, x]})#f] {
      def point[A](a: => A) =
        Suspend[({type f[x] = (=> S) => x})#f, A](s =>
          Return[({type f[x] = (=> S) => x})#f, A](a))
      def bind[A, B](s: Sink[S, A])(f: A => Sink[S, B]) = s flatMap f
    }
}

object Source {
  implicit def sourceMonad[S]: Monad[({type f[x] = Source[S, x]})#f] =
    new Monad[({type f[x] = Source[S, x]})#f] {
      override def point[A](a: => A) = Return[({type f[x] = (S, x)})#f, A](a)
      def bind[A, B](s: Source[S, A])(f: A => Source[S, B]) = s flatMap f
    }
}

object Coroutine extends Coroutines

trait Coroutines {
  implicit def tupleFunctor[S]: Functor[({type f[x] = (S, x)})#f] = new Functor[({type f[x] = (S, x)})#f] {
    def map[A, B](t: (S, A))(f: A => B): (S, B) =
      (t._1, f(t._2))
  }

  type Trampoline[A] = Coroutine[Function0, A]
  type Source[A, B] = Coroutine[({type f[x] = (A, x)})#f, B]
  type Sink[A, B] = Coroutine[({type f[x] = (=> A) => x})#f, B]

  def suspend[A](value: => A): Trampoline[A] =
    Suspend[Function0, A](() => Return[Function0, A](value))

  def pause: Trampoline[Unit] =
    suspend(())

  def produce[A](a: A): Source[A, Unit] =
    Suspend[({type f[x] = (A, x)})#f, Unit](a -> Return[({type f[x] = (A, x)})#f, Unit](()))

  def await[A]: Sink[A, A] =
    Suspend[({type f[x] = (=> A) => x})#f, A](a => Return[({type f[x] = (=> A) => x})#f, A](a))

  def request[I, O](x: I): Coroutine[({type f[x] = Request[I, O, x]})#f, O] =
    Suspend[({type f[x] = Request[I, O, x]})#f, O](Request(x, Return[({type f[x] = Request[I, O, x]})#f, O](_:O)))
}



