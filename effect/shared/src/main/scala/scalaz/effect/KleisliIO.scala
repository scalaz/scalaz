package scalaz.effect

/**
 * A `KleisliIO[E, A, B]` is an effectful function from `A` to `B`, which might
 * fail with an `E`.
 *
 * This is the moral equivalent of `A => IO[E, B]`, and, indeed, `KleisliIO`
 * extends this function type, and can be used in the same way.
 *
 * The main advantage to using `KleisliIO` is that it provides you a means of
 * importing an impure function `A => B` into `KleisliIO[E, A, B]`, without
 * actually wrapping the result of the function in an `IO` value.
 *
 * This allows the implementation to aggressively fuse operations on impure
 * functions, which in turn can result in significantly higher-performance and
 * far less heap utilization than equivalent approaches modeled with `IO`.
 *
 * The implementation allows you to lift functions from `A => IO[E, B]` into a
 * `KleisliIO[E, A, B]`. Such functions cannot be optimized, but will be handled
 * correctly and can work in conjunction with optimized (fused) `KleisliIO`.
 *
 * Those interested in learning more about modeling effects with `KleisliIO` are
 * encouraged to read John Hughes paper on the subject: Generalizing Monads to
 * Arrows (www.cse.chalmers.se/~rjmh/Papers/arrows.pdf). The implementation in
 * this file contains many of the same combinators as Hughes implementation.
 *
 * A word of warning: while even very complex code can be expressed in
 * `KleisliIO`, there is a point of diminishing return. If you find yourself
 * using deeply nested tuples to propagate information forward, it may be no
 * faster than using `IO`.
 *
 * Given the following two `KleisliIO`:
 *
 * {{{
 * val readLine = KleisliIO.impureVoid((_ : Unit) => scala.Console.readLine())
 * val printLine = KleisliIO.impureVoid((line: String) => println(line))
 * }}}
 *
 * Then the following two programs are equivalent:
 *
 * {{{
 * // Program 1
 * val program1: IO[Void, Unit] =
 *   for {
 *     name <- getStrLn
 *     _    <- putStrLn("Hello, " + name)
 *   } yield ())
 *
 * // Program 2
 * val program2: IO[Void, Unit] = (readLine >>> KleisliIO.lift("Hello, " + _) >>> printLine)(())
 * }}}
 *
 * Similarly, the following two programs are equivalent:
 *
 * {{{
 * // Program 1
 * val program1: IO[Void, Unit] =
 *   for {
 *     line1 <- getStrLn
 *     line2 <- getStrLn
 *     _     <- putStrLn("You wrote: " + line1 + ", " + line2)
 *   } yield ())
 *
 * // Program 2
 * val program2: IO[Void, Unit] =
 *   (readLine.zipWith(readLine)("You wrote: " + _ + ", " + _) >>> printLine)(())
 * }}}
 *
 * In both of these examples, the `KleisliIO` program is faster because it is
 * able to perform fusion of effectful functions.
 */
sealed trait KleisliIO[E, A, B] extends (A => IO[E, B]) { self =>

  /**
   * Applies the effectful function with the specified value, returning the
   * output in `IO`.
   */
  def apply(a: A): IO[E, B]

  /**
   * Maps the output of this effectful function by the specified function.
   */
  final def map[C](f: B => C): KleisliIO[E, A, C] = self >>> KleisliIO.lift(f)

  /**
   * Binds on the output of this effectful function.
   */
  final def flatMap[C](f: B => KleisliIO[E, A, C]): KleisliIO[E, A, C] =
    KleisliIO.flatMap(self, f)

  /**
   * Composes two effectful functions.
   */
  final def compose[A0](that: KleisliIO[E, A0, A]): KleisliIO[E, A0, B] =
    KleisliIO.compose(self, that)

  /**
   * "Backwards" composition of effectful functions.
   */
  final def andThen[C](that: KleisliIO[E, B, C]): KleisliIO[E, A, C] =
    that.compose(self)

  /**
   * A symbolic operator for `andThen`.
   */
  final def >>>[C](that: KleisliIO[E, B, C]): KleisliIO[E, A, C] =
    self.andThen(that)

  /**
   * Zips the output of this function with the output of that function, using
   * the specified combiner function.
   */
  final def zipWith[C, D](that: KleisliIO[E, A, C])(f: (B, C) => D): KleisliIO[E, A, D] =
    KleisliIO.zipWith(self, that)(f)

  /**
   * Returns a new effectful function that computes the value of this function,
   * storing it into the first element of a tuple, carrying along the input on
   * the second element of a tuple.
   */
  final def first: KleisliIO[E, A, (B, A)] =
    self &&& KleisliIO.identity[E, A]

  /**
   * Returns a new effectful function that computes the value of this function,
   * storing it into the second element of a tuple, carrying along the input on
   * the first element of a tuple.
   */
  final def second: KleisliIO[E, A, (A, B)] =
    KleisliIO.identity[E, A] &&& self

  /**
   * Returns a new effectful function that can either compute the value of this
   * effectful function (if passed `Left(a)`), or can carry along any other
   * `C` value (if passed `Right(c)`).
   */
  final def left[C]: KleisliIO[E, Either[A, C], Either[B, C]] =
    KleisliIO.left(self)

  /**
   * Returns a new effectful function that can either compute the value of this
   * effectful function (if passed `Right(a)`), or can carry along any other
   * `C` value (if passed `Left(c)`).
   */
  final def right[C]: KleisliIO[E, Either[C, A], Either[C, B]] =
    KleisliIO.right(self)

  /**
   * Returns a new effectful function that zips together the output of two
   * effectful functions that share the same input.
   */
  final def &&&[C](that: KleisliIO[E, A, C]): KleisliIO[E, A, (B, C)] =
    KleisliIO.zipWith(self, that)((a, b) => (a, b))

  /**
   * Returns a new effectful function that will either compute the value of this
   * effectful function (if passed `Left(a)`), or will compute the value of the
   * specified effectful function (if passed `Right(c)`).
   */
  final def |||[C](that: KleisliIO[E, C, B]): KleisliIO[E, Either[A, C], B] =
    KleisliIO.join(self, that)

  /**
   * Maps the output of this effectful function to the specified constant.
   */
  final def const[C](c: C): KleisliIO[E, A, C] =
    self >>> KleisliIO.lift[E, B, C](_ => c)

  /**
   * Maps the output of this effectful function to `Unit`.
   */
  final def toUnit: KleisliIO[E, A, Unit] = const(())

  /**
   * Returns a new effectful function that merely applies this one for its
   * effect, returning the input unmodified.
   */
  final def asEffect: KleisliIO[E, A, A] = self.first >>> KleisliIO._2
}

object KleisliIO {
  private class KleisliIOError[E](error: E) extends Throwable {
    def unsafeCoerce[E2] = error.asInstanceOf[E2]
  }

  private[effect] final class Pure[E, A, B](apply0: A => IO[E, B]) extends KleisliIO[E, A, B] {
    override final def apply(a: A): IO[E, B] = apply0(a)
  }
  private[effect] final class Impure[E, A, B](val apply0: A => B) extends KleisliIO[E, A, B] {
    override final def apply(a: A): IO[E, B] =
      IO.suspend {
        try IO.now[E, B](apply0(a))
        catch {
          case e: KleisliIOError[_] => IO.fail[E, B](e.unsafeCoerce[E])
        }
      }
  }

  /**
   * Lifts a value into the monad formed by `KleisliIO`.
   */
  final def point[E, A, B](b: => B): KleisliIO[E, A, B] = lift((_: A) => b)

  /**
   * Returns a `KleisliIO` representing a failure with the specified `E`.
   */
  final def fail[E, A, B](e: E): KleisliIO[E, A, B] =
    new Impure((a: A) => throw new KleisliIOError[E](e))

  /**
   * Returns the identity effectful function, which performs no effects and
   * merely returns its input unmodified.
   */
  final def identity[E, A]: KleisliIO[E, A, A] = lift((a: A) => a)

  /**
   * Lifts a pure `A => IO[E, B]` into `KleisliIO`.
   */
  final def pure[E, A, B](f: A => IO[E, B]): KleisliIO[E, A, B] = new Pure(f)

  /**
   * Lifts a pure `A => B` into `KleisliIO`.
   */
  final def lift[E, A, B](f: A => B): KleisliIO[E, A, B] = new Impure(f)

  /**
   * Returns an effectful function that merely swaps the elements in a `Tuple2`.
   */
  final def swap[E, A, B]: KleisliIO[E, (A, B), (B, A)] =
    KleisliIO.lift[E, (A, B), (B, A)](_.swap)

  /**
   * Lifts an impure function into `KleisliIO`, converting throwables into the
   * specified error type `E`.
   */
  final def impure[E, A, B](catcher: PartialFunction[Throwable, E])(f: A => B): KleisliIO[E, A, B] =
    new Impure(
      (a: A) =>
        try f(a)
        catch {
          case t: Throwable if (catcher.isDefinedAt(t)) =>
            throw new KleisliIOError(catcher(t))
      }
    )

  /**
   * Lifts an impure function into `KleisliIO`, assuming any throwables are
   * non-recoverable and do not need to be converted into errors.
   */
  final def impureVoid[A, B](f: A => B): KleisliIO[Void, A, B] = new Impure(f)

  /**
   * Returns a new effectful function that either applies the left function, if
   * the function is passed `Left(a)`, or applies the right function, if the
   * function is passed `Right(c)`.
   */
  final def switch[E, A, B, C](l: KleisliIO[E, A, B], r: KleisliIO[E, C, B]): KleisliIO[E, Either[A, C], B] =
    (l, r) match {
      case (l: Impure[E, A, B], r: Impure[E, C, B]) =>
        new Impure[E, Either[A, C], B]({
          case Left(a)  => l.apply0(a)
          case Right(c) => r.apply0(c)
        })
      case _ =>
        KleisliIO.pure[E, Either[A, C], B] {
          case Left(a)  => l(a)
          case Right(c) => r(c)
        }
    }

  /**
   * Returns a new effectful function that passes an `A` to the condition, and
   * if the condition returns true, returns `Left(a)`, but if the condition
   * returns false, returns `Right(a)`.
   */
  final def test[E, A](k: KleisliIO[E, A, Boolean]): KleisliIO[E, A, Either[A, A]] =
    (k &&& KleisliIO.identity[E, A]) >>>
      KleisliIO.lift((t: (Boolean, A)) => if (t._1) Left(t._2) else Right(t._2))

  /**
   * Returns a new effectful function that passes an `A` to the condition, and
   * if the condition returns true, passes the `A` to the `then0` function,
   * but if the condition returns false, passes the `A` to the `else0` function.
   */
  final def ifThenElse[E, A, B](
    cond: KleisliIO[E, A, Boolean]
  )(then0: KleisliIO[E, A, B])(else0: KleisliIO[E, A, B]): KleisliIO[E, A, B] =
    (cond, then0, else0) match {
      case (cond: Impure[_, _, _], then0: Impure[_, _, _], else0: Impure[_, _, _]) =>
        new Impure[E, A, B](a => if (cond.apply0(a)) then0.apply0(a) else else0.apply0(a))
      case _ => test[E, A](cond) >>> switch(then0, else0)
    }

  /**
   * Returns a new effectful function that passes an `A` to the condition, and
   * if the condition returns true, passes the `A` to the `then0` function, but
   * otherwise returns the original `A` unmodified.
   */
  final def ifThen[E, A](cond: KleisliIO[E, A, Boolean])(then0: KleisliIO[E, A, A]): KleisliIO[E, A, A] =
    (cond, then0) match {
      case (cond: Impure[_, _, _], then0: Impure[_, _, _]) =>
        new Impure[E, A, A](a => if (cond.apply0(a)) then0.apply0(a) else a)
      case _ => ifThenElse(cond)(then0)(KleisliIO.identity[E, A])
    }

  /**
   * Returns a new effectful function that passes an `A` to the condition, and
   * if the condition returns false, passes the `A` to the `then0` function, but
   * otherwise returns the original `A` unmodified.
   */
  final def ifNotThen[E, A](cond: KleisliIO[E, A, Boolean])(then0: KleisliIO[E, A, A]): KleisliIO[E, A, A] =
    (cond, then0) match {
      case (cond: Impure[_, _, _], then0: Impure[_, _, _]) =>
        new Impure[E, A, A](a => if (cond.apply0(a)) a else then0.apply0(a))
      case _ => ifThenElse(cond)(KleisliIO.identity[E, A])(then0)
    }

  /**
   * Returns a new effectful function that passes an `A` to the condition, and
   * if the condition returns true, passes the `A` through the body to yield a
   * new `A`, which repeats until the condition returns false. This is the
   * `KleisliIO` equivalent of a `while(cond) { body }` loop.
   */
  final def whileDo[E, A](check: KleisliIO[E, A, Boolean])(body: KleisliIO[E, A, A]): KleisliIO[E, A, A] =
    (check, body) match {
      case (check: Impure[_, _, _], body: Impure[_, _, _]) =>
        new Impure[E, A, A]({ (a0: A) =>
          var a = a0

          val cond   = check.apply0
          val update = body.apply0

          while (cond(a)) {
            a = update(a)
          }

          a
        })

      case _ =>
        lazy val loop: KleisliIO[E, A, A] =
          KleisliIO.pure((a: A) => check(a).flatMap((b: Boolean) => if (b) body(a).flatMap(loop) else IO.now(a)))

        loop
    }

  /**
   * Returns an effectful function that extracts out the first element of a
   * tuple.
   */
  def _1[E, A, B]: KleisliIO[E, (A, B), A] = lift[E, (A, B), A](_._1)

  /**
   * Returns an effectful function that extracts out the second element of a
   * tuple.
   */
  def _2[E, A, B]: KleisliIO[E, (A, B), B] = lift[E, (A, B), B](_._2)

  /**
   * See @KleisliIO.flatMap
   */
  final def flatMap[E, A, B, C](fa: KleisliIO[E, A, B], f: B => KleisliIO[E, A, C]): KleisliIO[E, A, C] =
    new Pure((a: A) => fa(a).flatMap(b => f(b)(a)))

  /**
   * See KleisliIO.compose
   */
  final def compose[E, A, B, C](second: KleisliIO[E, B, C], first: KleisliIO[E, A, B]): KleisliIO[E, A, C] =
    (second, first) match {
      case (second: Impure[_, _, _], first: Impure[_, _, _]) =>
        new Impure(second.apply0.compose(first.apply0))

      case _ =>
        new Pure((a: A) => first(a).flatMap(second))
    }

  /**
   * See KleisliIO.zipWith
   */
  final def zipWith[E, A, B, C, D](l: KleisliIO[E, A, B], r: KleisliIO[E, A, C])(f: (B, C) => D): KleisliIO[E, A, D] =
    (l, r) match {
      case (l: Impure[_, _, _], r: Impure[_, _, _]) =>
        new Impure((a: A) => {
          val b = l.apply0(a)
          val c = r.apply0(a)

          f(b, c)
        })

      case _ =>
        KleisliIO.pure(
          (a: A) =>
            for {
              b <- l(a)
              c <- r(a)
            } yield f(b, c)
        )
    }

  /**
   * See KleisliIO.left
   */
  final def left[E, A, B, C](k: KleisliIO[E, A, B]): KleisliIO[E, Either[A, C], Either[B, C]] =
    k match {
      case k: Impure[E, A, B] =>
        new Impure[E, Either[A, C], Either[B, C]]({
          case Left(a)  => Left(k.apply0(a))
          case Right(c) => Right(c)
        })
      case _ =>
        KleisliIO.pure[E, Either[A, C], Either[B, C]] {
          case Left(a)  => k(a).map[Either[B, C]](Left[B, C])
          case Right(c) => IO.now[E, Either[B, C]](Right(c))
        }
    }

  /**
   * See KleisliIO.left
   */
  final def right[E, A, B, C](k: KleisliIO[E, A, B]): KleisliIO[E, Either[C, A], Either[C, B]] =
    k match {
      case k: Impure[E, A, B] =>
        new Impure[E, Either[C, A], Either[C, B]]({
          case Left(c)  => Left(c)
          case Right(a) => Right(k.apply0(a))
        })
      case _ =>
        KleisliIO.pure[E, Either[C, A], Either[C, B]] {
          case Left(c)  => IO.now[E, Either[C, B]](Left(c))
          case Right(a) => k(a).map[Either[C, B]](Right[C, B])
        }
    }

  /**
   * See KleisliIO.|||
   */
  final def join[E, A, B, C](l: KleisliIO[E, A, B], r: KleisliIO[E, C, B]): KleisliIO[E, Either[A, C], B] =
    (l, r) match {
      case (l: Impure[_, _, _], r: Impure[_, _, _]) =>
        new Impure[E, Either[A, C], B]({
          case Left(a)  => l.apply0(a)
          case Right(c) => r.apply0(c)
        })

      case _ =>
        KleisliIO.pure[E, Either[A, C], B]({
          case Left(a)  => l(a)
          case Right(c) => r(c)
        })
    }
}
