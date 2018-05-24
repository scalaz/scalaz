package scalaz.effect

/**
 * An effectful function from `A` to `B`, which might error with an `E`.
 *
 * {{{
 * val readLine = KleisliIO.impureVoid((_ : Unit) => scala.Console.readLine())
 * val printLine = KleisliIO.impureVoid((line: String) => println(line))
 * }}}
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
 * {{{
 *
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
 */
sealed trait KleisliIO[E, A, B] extends (A => IO[E, B]) { self =>
  def apply(a: A): IO[E, B]

  final def map[C](f: B => C): KleisliIO[E, A, C] = self >>> KleisliIO.lift(f)

  final def flatMap[C](f: B => KleisliIO[E, A, C]): KleisliIO[E, A, C] =
    KleisliIO.pure((a: A) => self(a).flatMap(b => f(b)(a)))

  /**
   * Given an effectful function A0 => A, and an effectful function A => B,
   * composes them to yield an effectful function A0 => B.
   */
  final def compose[A0](that: KleisliIO[E, A0, A]): KleisliIO[E, A0, B] =
    KleisliIO.compose(self, that)

  final def andThen[C](that: KleisliIO[E, B, C]): KleisliIO[E, A, C] =
    that.compose(self)

  final def >>>[C](that: KleisliIO[E, B, C]): KleisliIO[E, A, C] =
    that.compose(self)

  final def zipWith[C, D](that: KleisliIO[E, A, C])(f: (B, C) => D): KleisliIO[E, A, D] =
    KleisliIO.zipWith(self, that)(f)

  final def first: KleisliIO[E, A, (B, A)] =
    self &&& KleisliIO.identity[E, A]

  final def second: KleisliIO[E, A, (A, B)] =
    KleisliIO.identity[E, A] &&& self

  final def left[C]: KleisliIO[E, Either[A, C], Either[B, C]] =
    KleisliIO.left(self)

  final def right[C]: KleisliIO[E, Either[C, A], Either[C, B]] =
    KleisliIO.right(self)

  final def &&&[C](that: KleisliIO[E, A, C]): KleisliIO[E, A, (B, C)] =
    KleisliIO.zipWith(self, that)((a, b) => (a, b))

  final def |||[C](that: KleisliIO[E, C, B]): KleisliIO[E, Either[A, C], B] =
    KleisliIO.join(self, that)

  final def const[C](c: C): KleisliIO[E, A, C] =
    self >>> KleisliIO.lift[E, B, C](_ => c)

  final def toUnit: KleisliIO[E, A, Unit] = const(())

  final def asEffect: KleisliIO[E, A, A] = self.first >>> KleisliIO._2
}

object KleisliIO {
  final val VoidCatcher: PartialFunction[Throwable, Void] = PartialFunction.empty[Throwable, Void]

  private class KleisliIOError[E](error: E) extends Throwable {
    def unsafeCoerce[E2] = error.asInstanceOf[E2]
  }

  private[effect] final class Pure[E, A, B](apply0: A => IO[E, B]) extends KleisliIO[E, A, B] {
    override final def apply(a: A): IO[E, B] = apply0(a)
  }
  private[effect] final class Impure[E, A, B](val apply0: A => B) extends KleisliIO[E, A, B] {
    override final def apply(a: A): IO[E, B] =
      try {
        IO.sync[E, B](apply0(a))
      } catch {
        case e: KleisliIOError[_] => IO.fail[E, B](e.unsafeCoerce[E])
      }
  }
  private[effect] final class Compose[E, A, B, C](second: KleisliIO[E, B, C], first: KleisliIO[E, A, B])
      extends KleisliIO[E, A, C] {
    override final def apply(a: A): IO[E, C] = first(a).flatMap(b => second(b))
  }

  final def point[E, A, B](b: => B): KleisliIO[E, A, B] = lift((_: A) => b)

  final def fail[E, A, B](e: E): KleisliIO[E, A, B] =
    new Impure((a: A) => throw new KleisliIOError[E](e))

  final def identity[E, A]: KleisliIO[E, A, A] = lift((a: A) => a)

  final def pure[E, A, B](f: A => IO[E, B]): KleisliIO[E, A, B] = new Pure(f)

  final def lift[E, A, B](f: A => B): KleisliIO[E, A, B] = new Impure(f)

  final def swap[E, A, B]: KleisliIO[E, (A, B), (B, A)] =
    KleisliIO.lift[E, (A, B), (B, A)](_.swap)

  final def impure[E, A, B](catcher: PartialFunction[Throwable, E])(f: A => B): KleisliIO[E, A, B] =
    new Impure(
      (a: A) =>
        try f(a)
        catch {
          case t: Throwable if (catcher.isDefinedAt(t)) =>
            throw new KleisliIOError(catcher(t))
      }
    )

  final def impureVoid[A, B](f: A => B): KleisliIO[Void, A, B] = new Impure(f)

  final def compose[E, A, B, C](second: KleisliIO[E, B, C], first: KleisliIO[E, A, B]): KleisliIO[E, A, C] =
    (second, first) match {
      case (second: Impure[_, _, _], first: Impure[_, _, _]) =>
        new Impure(second.apply0.compose(first.apply0))

      case _ =>
        new Compose(second, first)
    }

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

  final def test[E, A](k: KleisliIO[E, A, Boolean]): KleisliIO[E, A, Either[A, A]] =
    (k &&& KleisliIO.identity[E, A]) >>>
      KleisliIO.lift((t: (Boolean, A)) => if (t._1) Left(t._2) else Right(t._2))

  final def ifThenElse[E, A, B](
    cond: KleisliIO[E, A, Boolean]
  )(then0: KleisliIO[E, A, B])(else0: KleisliIO[E, A, B]): KleisliIO[E, A, B] =
    (cond, then0, else0) match {
      case (cond: Impure[_, _, _], then0: Impure[_, _, _], else0: Impure[_, _, _]) =>
        new Impure[E, A, B](a => if (cond.apply0(a)) then0.apply0(a) else else0.apply0(a))
      case _ => test[E, A](cond) >>> switch(then0, else0)
    }

  final def ifThen[E, A, B](cond: KleisliIO[E, A, Boolean])(then0: KleisliIO[E, A, A]): KleisliIO[E, A, A] =
    ifThenElse(cond)(then0)(KleisliIO.identity[E, A])

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

  def whileDo[E, A](check: KleisliIO[E, A, Boolean])(body: KleisliIO[E, A, A]): KleisliIO[E, A, A] =
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

  def _1[E, A, B]: KleisliIO[E, (A, B), A] = lift[E, (A, B), A](_._1)
  def _2[E, A, B]: KleisliIO[E, (A, B), B] = lift[E, (A, B), B](_._2)
}
