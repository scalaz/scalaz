package scalaz
package effects

import Scalaz._

sealed trait IO[A] {
  private[effects] def apply(rw: World[RealWorld]): (World[RealWorld], A)
  /**
   * Unsafe operation. Runs I/O and performs side-effects.
   * Do not call until the end of the universe.
   */
  def unsafePerformIO: A = apply(realWorld)._2

  def flatMap[B](f: A => IO[B]): IO[B] = IO(rw => {
    val (nw, a) = apply(rw)
    f(a)(nw)
  })

  def map[B](f: A => B): IO[B] = IO(rw => {
    val (nw, a) = apply(rw)
    (nw, f(a))
  })

  def liftIO[M[_]](implicit m: MonadIO[M]): M[A] = m.liftIO(this)  
  
  /** Executes the handler if an exception is raised. */
  def except(handler: Throwable => IO[A]): IO[A] = 
    IO(rw => try { this(rw) } catch { case e => handler(e)(rw) })

  /**
   * Executes the handler for exceptions that are raised and match the given predicate.
   * Other exceptions are rethrown.
   */
  def catchSome[B](p: Throwable => Option[B], handler: B => IO[A]): IO[A] =
    except(e => p(e) cata (handler, throw e))

  /**
   * Returns an Either result which is Right if no exception was raised, or Left if an
   * exception was raised.
   */
  def catchLeft: IO[Either[Throwable, A]] = map(_.right[Throwable]) except (_.left.pure[IO])

  /** Like "catchLeft" but takes a predicate to select which exceptions are caught. */
  def catchSomeLeft[B](p: Throwable => Option[B]): IO[Either[B, A]] = for {
    r <- this.catchLeft
    x <- r match {
      case Right(v) => Right(v).pure[IO]
      case Left(e) => p(e) cata (b => Left[B, A](b).pure[IO], throw e)
    }
  } yield x

  /** Like "finally", but only performs the final action if there was an exception. */
  def onException[B](action: IO[B]): IO[A] = this except (e => for {
    _ <- action
    a <- (throw e) : IO[A]
  } yield a)

  /**
   * Applies the "during" action, calling "after" regardless of whether there was an exception.
   * All exceptions are rethrown. Generalizes try/finally.
   */
  def bracket[B, C](after: A => IO[B])(during: A => IO[C]): IO[C] = for {
    a <- this
    r <- during(a) onException after(a)
    _ <- after(a)
  } yield r

  /** Like "bracket", but takes only a computation to run afterward. Generalizes "finally". */
  def ensuring[B](sequel: IO[B]): IO[A] = for {
    r <- onException(sequel)
    _ <- sequel
  } yield r

  /** A variant of "bracket" where the return value of this computation is not needed. */
  def bracket_[B, C](after: IO[B])(during: IO[C]): IO[C] =
    bracket(_ => after)(_ => during)

  /** A variant of "bracket" that performs the final action only if there was an error. */
  def bracketOnError[B, C](after: A => IO[B])(during: A => IO[C]): IO[C] = for {
    a <- this
    r <- during(a) onException after(a)
  } yield r

  def bracketIO[M[_], B](after: A => IO[Unit])(during: A => M[B])(implicit m: MonadControlIO[M]): M[B] =
    controlIO((runInIO: RunInBase[M, IO]) => bracket(after)(runInIO.apply compose during))
}

/** 
 * A mutable reference in the IO monad. Note that unsafePerformIO will allow leaking 
 * such a reference out of the monad, but any operations on a leaked reference are still monadic.
 */
class IORef[A](val value: STRef[RealWorld, A]) extends NewType[STRef[RealWorld, A]] {
  def read: IO[A] = stToIO(value.read)
  def write(a: => A): IO[Unit] = stToIO(value.write(a) map (_ => ()))
  def mod(f: A => A): IO[A] = stToIO(value.mod(f) >>= (_.read))
}

object IO {
  def apply[A](f: World[RealWorld] => (World[RealWorld], A)): IO[A] = new IO[A] {
    private[effects] def apply(rw: World[RealWorld]) = f(rw)
  }

  implicit val ioPure: Pure[IO] = new Pure[IO] {
    def pure[A](a: => A) = IO(rw => (rw, a))
  }
  implicit val ioBind: Bind[IO] = new Bind[IO] {
    def bind[A, B](io: IO[A], f: A => IO[B]): IO[B] = io flatMap f
  }

  implicit val ioFunctor: Functor[IO] = new Functor[IO] {
    def fmap[A, B](io: IO[A], f: A => B): IO[B] = io map f 
  }
}
