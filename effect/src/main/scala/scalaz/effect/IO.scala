package scalaz
package effect

import IvoryTower._
import RegionT._
import RefCountedFinalizer._
import FinalizerHandle._
import ST._
import Kleisli._
import Free._
import std.function._

sealed trait IO[+A] {
  private[effect] def apply(rw: Tower[IvoryTower]): Trampoline[(Tower[IvoryTower], A)]

  import IO._

  /**
   * Runs I/O and performs side-effects. An unsafe operation.
   * Do not call until the end of the universe.
   */
  def unsafePerformIO(): A = apply(ivoryTower).run._2

  /**
   * Constructs an IO action whose steps may be interleaved with another.
   * An unsafe operation, since it exposes a trampoline that allows one to
   * step through the components of the IO action.
   */
  def unsafeInterleaveIO(): IO[Trampoline[A]] = IO(apply(ivoryTower).map(_._2))

  /**
   * Interleaves the steps of this IO action with the steps of another,
   * consuming the results of both with the given function.
   */
  def unsafeZipWith[B, C](iob: IO[B], f: (A, B) => C): IO[C] = (for {
    a <- unsafeInterleaveIO()
    b <- iob.unsafeInterleaveIO()
    c <- io(rw => a zipWith (b, (x: A, y: B) => (rw -> f(x, y))))
  } yield c)

  /**
   * Interleaves the steps of this IO action with the steps of another,
   * yielding the results of both.
   */
  def unsafeZip[B](iob: IO[B]): IO[(A, B)] = unsafeZipWith(iob, Tuple2[A, B])

  /**
   * Interleaves the steps of this IO action with the steps of another,
   * ignoring the result of this action.
   */
  def unsafeZip_[B](iob: IO[B]): IO[B] = unsafeZipWith(iob, (a: A, b: B) => b)

  /** Continues this action with the given function. */
  def map[B](f: A => B): IO[B] = io(rw =>
    apply(rw) map {
      case (nw, a) => (nw, f(a))
    })

  /** Continues this action with the given action. */
  def flatMap[B](f: A => IO[B]): IO[B] = io(rw =>
    apply(rw) flatMap {
      case (nw, a) => f(a)(nw)
    })

  /** Lift this action to a given IO-like monad. */
  def liftIO[M[+_]](implicit m: MonadIO[M]): M[A] =
    m.liftIO(this)

  /** Executes the handler if an exception is raised. */
  def except[B >: A](handler: Throwable => IO[B]): IO[B] = 
    io(rw => try { Return(this(rw).run) } catch { case e: Throwable => handler(e)(rw) })

  /**
   * Executes the handler for exceptions that are raised and match the given predicate.
   * Other exceptions are rethrown.
   */
  def catchSome[B,C >: A](p: Throwable => Option[B], handler: B => IO[C]): IO[C] =
    except(e => p(e) match {
      case Some(z) => handler(z)
      case None => throw e
    })

  /**
   * Returns a disjunction result which is right if no exception was raised, or left if an
   * exception was raised.
   */
  def catchLeft: IO[Throwable \/ A] =
    map(\/.right) except (t => IO(\/.left(t)))

  /**Like "catchLeft" but takes a predicate to select which exceptions are caught. */
  def catchSomeLeft[B](p: Throwable => Option[B]): IO[B \/ A] =
    catchLeft map (_.leftMap(e => p(e).getOrElse(throw e)))

  /**Like "finally", but only performs the final action if there was an exception. */
  def onException[B](action: IO[B]): IO[A] = this except (e => for {
    _ <- action
    a <- (throw e): IO[A]
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

  /**Like "bracket", but takes only a computation to run afterward. Generalizes "finally". */
  def ensuring[B](sequel: IO[B]): IO[A] = for {
    r <- onException(sequel)
    _ <- sequel
  } yield r

  /**A variant of "bracket" where the return value of this computation is not needed. */
  def bracket_[B, C](after: IO[B])(during: IO[C]): IO[C] =
    bracket(_ => after)(_ => during)

  /**A variant of "bracket" that performs the final action only if there was an error. */
  def bracketOnError[B, C](after: A => IO[B])(during: A => IO[C]): IO[C] = for {
    a <- this
    r <- during(a) onException after(a)
  } yield r

  def bracketIO[M[_], B](after: A => IO[Unit])(during: A => M[B])(implicit m: MonadControlIO[M]): M[B] =
    controlIO((runInIO: RunInBase[M, IO]) => bracket(after)(runInIO.apply compose during))

  /** An automatic resource management. */
  def using[B >: A, C](f: B => IO[C])(implicit resource: Resource[B]) =
    bracket(resource.close)(f)
}

object IO extends IOFunctions with IOInstances {
  def apply[A](a: => A): IO[A] =
    io(rw => return_(rw -> a))
}

trait IOInstances1 {
  implicit def IOSemigroup[A](implicit A: Semigroup[A]): Semigroup[IO[A]] =
      Monoid.liftSemigroup[IO, A](IO.ioMonad, A)

  implicit val iOLiftIO: LiftIO[IO] = new IOLiftIO {}

  implicit val ioMonad: Monad[IO] = new IOMonad {}
}

trait IOInstances0 extends IOInstances1 {
  implicit def IOMonoid[A](implicit A: Monoid[A]): Monoid[IO[A]] =
    Monoid.liftMonoid[IO, A](ioMonad, A)
  
  implicit val ioMonadIO: MonadIO[IO] = new MonadIO[IO] with IOLiftIO with IOMonad
}

trait IOInstances extends IOInstances0 {
  implicit val ioMonadCatchIO: MonadCatchIO[IO] = new IOMonadCatchIO with IOLiftIO with IOMonad
}

private trait IOMonad extends Monad[IO] {
  def point[A](a: => A): IO[A] = IO(a)
  override def map[A, B](fa: IO[A])(f: A => B) = fa map f
  def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
}

private trait IOLiftIO extends LiftIO[IO] {
  def liftIO[A](ioa: IO[A]) = ioa
}

private trait IOMonadCatchIO extends MonadCatchIO[IO] {
  def except[A](io: IO[A])(h: Throwable ⇒ IO[A]): IO[A] = io.except(h)
}

/** IO Actions for writing to standard output and and reading from standard input */
trait IOStd {
  import IO.io

  /** Reads a character from standard input. */
  def getChar: IO[Char] = IO(readChar())

  /** Writes a character to standard output. */
  def putChar(c: Char): IO[Unit] = io(rw => return_(rw -> {
    print(c)
    ()
  }))

  /** Writes a string to standard output. */
  def putStr(s: String): IO[Unit] = io(rw => return_(rw -> {
    print(s)
    ()
  }))

  /** Writes a string to standard output, followed by a newline.*/
  def putStrLn(s: String): IO[Unit] = io(rw => return_(rw -> {
    println(s)
    ()
  }))

  /** Reads a line of standard input. */
  def readLn: IO[String] = IO(readLine())

  def put[A](a: A)(implicit S: Show[A]): IO[Unit] =
    io(rw => return_(rw -> {
      print(S shows a)
      ()
    }))

  def putLn[A](a: A)(implicit S: Show[A]): IO[Unit] =
    io(rw => return_(rw -> {
      println(S shows a)
      ()
    }))

}

trait IOFunctions extends IOStd {
  type RunInBase[M[_], Base[_]] =
  Forall[({type λ[B] = M[B] => Base[M[B]]})#λ]

  /** Construct an IO action from a world-transition function. */
  def io[A](f: Tower[IvoryTower] => Trampoline[(Tower[IvoryTower], A)]): IO[A] = new IO[A] {
    private[effect] def apply(rw: Tower[IvoryTower]) = f(rw)
  }

  // Mutable variables in the IO monad
  def newIORef[A](a: => A): IO[IORef[A]] =
    STToIO(newVar(a)) flatMap (v => IO(IORef.ioRef(v)))

  /**Throw the given error in the IO monad. */
  def throwIO[A](e: Throwable): IO[A] = IO(throw e)

  def idLiftControl[M[_], A](f: RunInBase[M, M] => M[A])(implicit m: Monad[M]): M[A] =
    f(new RunInBase[M, M] {
      def apply[B] = (x: M[B]) => m.point(x)
    })

  def controlIO[M[_], A](f: RunInBase[M, IO] => IO[M[A]])(implicit M: MonadControlIO[M]): M[A] =
    M.join(M.liftControlIO(f))

  /**
   * Register a finalizer in the current region. When the region terminates,
   * all registered finalizers will be performed if they're not duplicated to a parent region.
   */
  def onExit[S, P[+_] : MonadIO](finalizer: IO[Unit]):
  RegionT[S, P, FinalizerHandle[({type λ[α] = RegionT[S, P, α]})#λ]] =
    regionT(kleisli(hsIORef => (for {
      refCntIORef <- newIORef(1)
      h = refCountedFinalizer(finalizer, refCntIORef)
      _ <- hsIORef.mod(h :: _)
    } yield finalizerHandle[({type λ[α] = RegionT[S, P, α]})#λ](h)).liftIO[P]))

  /**
   * Execute a region inside its parent region P. All resources which have been opened in the given
   * region and which haven't been duplicated using "dup", will be closed on exit from this function
   * whether by normal termination or by raising an exception.
   * Also all resources which have been duplicated to this region from a child region are closed
   * on exit if they haven't been duplicated themselves.
   * The Forall quantifier prevents resources from being returned by this function.
   */
  def runRegionT[P[+_] : MonadControlIO, A](r: Forall[({type λ[S] = RegionT[S, P, A]})#λ]): P[A] = {
    def after(hsIORef: IORef[List[RefCountedFinalizer]]) = for {
      hs <- hsIORef.read
      _ <- hs.foldRight[IO[Unit]](IO.ioUnit) {
        case (r, o) => for {
          refCnt <- r.refcount.mod(_ - 1)
          _ <- if (refCnt == 0) r.finalizer else IO.ioUnit
        } yield ()
      }
    } yield ()
    newIORef(List[RefCountedFinalizer]()).bracketIO(after)(s => r.apply.value.run(s))
  }

  /** An IO action is an ST action. */
  implicit def IOToST[A](io: IO[A]): ST[IvoryTower, A] =
    st(io(_).run)

  /** An IO action that does nothing. */
  val ioUnit: IO[Unit] =
    IO(())
}
