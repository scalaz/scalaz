package scalaz

import Free._
import std.function._

package object coroutine {

  /** A sink can always produce an answer of type `B`, and it can always receive a value of type `A`.
   *  It may refine the answer based on the values it receives. */
  type Sink[A, B] = Cofree[({type λ[α] = A => α})#λ, B]
  
  /** A source produces values of type `A` until it terminates with a value of type `B`.*/
  type Source[A, B] = Free[({type λ[α] = () => (A, α)})#λ, B]

  /** An iteratee that can consume values of type `A` until it terminates with a value of type `B`. */
  type Iteratee[A, B] = Free[({type λ[α] = A => α})#λ, B]

  /** A pipe can request values of type `A` or produce values of type `B`, until it terminates with 
    * a value of type `C`. A `Pipe[A, B, C]` can be driven with a `Sink[B, (A, D)]` or a pair of
    * `(Sink[B, D], Source[A, E])`. */
  type Pipe[A, B, C] = Free[({type λ[α] = Either[A => α, () => (B, α)]})#λ, C]

  /** Produces values of types `A` and `B`, indefinitely. Can be used to drive an iteratee. */
  type Coiteratee[A, B] = Cofree[({type λ[α] = () => (A, α)})#λ, B]

  /** Produces a `C` at every step, and either requests an `A` or produces a `B`. */
  type Conduit[A, B, C] = Cofree[({type λ[α] = Either[A => α, (B, α)]})#λ, C]

  // A sink can drive a source by zapping.

  /** 
   * A pipe in front of a sink is a sink.
   * Note: This combinator will drain the pipe into the sink immediately. Probably want to make sinks
   * monadic to delay this effect.
   */
  implicit def pipeSink[A, B, C, E](pipe: Pipe[A, B, E],
                                    sink: Sink[B, C])(f: (E, C) => C): Sink[A, C] =
    pipe.resume match {
      // This pipe has terminated. Return a sink that produces the same answer even if you keep feeding it.
      case Right(e) => {
        val r: Sink[A, C] = new Sink(f(e, sink.head), (a: A) => r)
        r
      }
      // The pipe has produced a value. Drain the pipe into the sink.
      case Left(Right(h)) => {
        val (b, n) = h()
        val m = sink.tail(b)
        Cofree(m.head, (a: A) => pipeSink(n, m.tail(a)))
      }
      // The pipe requests more input. Return a sink whose input feeds into the pipe.
      case (Left(Left(h)), m@Cofree(d, _)) =>
        Cofree(d, a => pipeSink(h(a), m))
    }

  /** 
   * A pipe behind a source is a source. Note: Will feed the pipe immediately if there is data in the source.
   * Probably want to suspend the source in a monad.
   */
  implicit def pipeSource[A, B, E: Semigroup](source: Source[A, E],
                                              pipe: Pipe[A, B, E]): Source[B, E] =
    (source.resume, pipe.resume) match {
      case (Left(e1), Left(e2)) => return_(e1 |+| e2)
      case (_, Left(e)) => Return(e)
      case (Left(e), _) => Return(e)
      case (r, Right(Right(k))) => Suspend(() => {
        val (b, n) = k()
        (b, pipeSource(r, n))
      })
      case (Right(h), Right(Left(k))) => {
        val (a, n) = h()
        pipeSource(n, k(a))
      }
    }

  /** Pipes form a category if the termination signals can be composed. **/
  implicit def pipeCategory[E: Semigroup]: Category[({type λ[α,β] = Pipe[α, β, E]})#λ] =
    new Category[({type λ[α,β] = Pipe[α, β, E]})#λ] {
      def id[A] = Suspend(Left(a => Suspend(Right(() => (a, id)))))
      def compose[X, Y, Z](f: Pipe[Y, Z, E], g: Pipe[X, Y, E]): Pipe[X, Z, E] =
        (f.resume, g.resume) match {
          case (Left(e1), Left(e2)) => Done(e1 |+| e2)
          case (Left(e), _) => Done(e)
          case (_, Left(e)) => Done(e)
          // Arbitrarily choosing right-then-left
          case (Right(Right(h)), Right(Left(k))) => Suspend(Right(() => {
            val (z, n) = h()
            (z, Suspend(Left(x => compose(n, k(x)))))
          }))
          case (Right(Right(h)), m) => Suspend(Right(() => {
            val (z, n) = h()
            (z, compose(n, m))
          }))
          case (n, Right(Left(k))) => Suspend(Left(x => compose(n, k(x))))
        }
    }
}
