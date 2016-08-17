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

sealed abstract class IO[A] {
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
    c <- io(rw => a.zipWith(b)((x, y) => (rw -> f(x, y))))
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
  def liftIO[M[_]](implicit m: MonadIO[M]): M[A] =
    m.liftIO(this)

  /** Executes the handler if an exception is raised. */
  def except(handler: Throwable => IO[A]): IO[A] =
    io(rw => try { Free.pure(this(rw).run) } catch { case e: Throwable => handler(e)(rw) })

  /**
   * Executes the handler for exceptions that are raised and match the given predicate.
   * Other exceptions are rethrown.
   */
  def catchSome[B](p: Throwable => Option[B], handler: B => IO[A]): IO[A] =
    except(e => p(e) match {
      case Some(z) => handler(z)
      case None => throw e
    })

  /**
   * Returns a disjunction result which is right if no exception was raised, or left if an
   * exception was raised.
   */
  def catchLeft: IO[Throwable \/ A] =
    map(\/.right[Throwable, A]) except (t => IO(-\/(t)))

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
  def using[C](f: A => IO[C])(implicit resource: Resource[A]) =
    bracket(resource.close)(f)
}

sealed abstract class IOInstances1 {
  implicit def IOSemigroup[A](implicit A: Semigroup[A]): Semigroup[IO[A]] =
      Semigroup.liftSemigroup[IO, A](IO.ioMonad, A)

  implicit val iOLiftIO: LiftIO[IO] = new IOLiftIO {}

  implicit val ioMonad: Monad[IO] with BindRec[IO] = new IOMonad {}
}

sealed abstract class IOInstances0 extends IOInstances1 {
  implicit def IOMonoid[A](implicit A: Monoid[A]): Monoid[IO[A]] =
    Monoid.liftMonoid[IO, A](ioMonad, A)

  implicit val ioMonadIO: MonadIO[IO] = new MonadIO[IO] with IOLiftIO with IOMonad
}

sealed abstract class IOInstances extends IOInstances0 {
  implicit val ioMonadCatchIO: MonadCatchIO[IO] = new IOMonadCatchIO with IOLiftIO with IOMonad

  implicit val ioCatchable: Catchable[IO] =
    new Catchable[IO] {
      def attempt[A](f: IO[A]): IO[Throwable \/ A] = f.catchLeft
      def fail[A](err: Throwable): IO[A] = IO(throw err)
    }

}

private trait IOMonad extends Monad[IO] with BindRec[IO] {
  def point[A](a: => A): IO[A] = IO(a)
  override def map[A, B](fa: IO[A])(f: A => B) = fa map f
  def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
  def tailrecM[A, B](f: A => IO[A \/ B])(a: A): IO[B] = IO.tailrecM(f)(a)
}

private trait IOLiftIO extends LiftIO[IO] {
  def liftIO[A](ioa: IO[A]) = ioa
}

private trait IOMonadCatchIO extends MonadCatchIO[IO] {
  def except[A](io: IO[A])(h: Throwable => IO[A]): IO[A] = io.except(h)
}

object IO extends IOInstances {
  def apply[A](a: => A): IO[A] =
    io(rw => return_(rw -> a))

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

  type RunInBase[M[_], Base[_]] =
  Forall[λ[α => M[α] => Base[M[α]]]]

  import scalaz.Isomorphism.<~>

  /** Hoist RunInBase given a natural isomorphism between the two functors */
  def hoistRunInBase[F[_], G[_]](r: RunInBase[G, IO])(implicit iso: F <~> G): RunInBase[F, IO] =
    new RunInBase[F, IO] {
      def apply[B] = (x: F[B]) => r.apply(iso.to(x)).map(iso.from(_))
    }

  /** Construct an IO action from a world-transition function. */
  def io[A](f: Tower[IvoryTower] => Trampoline[(Tower[IvoryTower], A)]): IO[A] =
    new IO[A] {
      private[effect] def apply(rw: Tower[IvoryTower]) = Free(() => f(rw))
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
  def onExit[S, P[_] : MonadIO](finalizer: IO[Unit]):
  RegionT[S, P, FinalizerHandle[RegionT[S, P, ?]]] =
    regionT(kleisli(hsIORef => (for {
      refCntIORef <- newIORef(1)
      h = refCountedFinalizer(finalizer, refCntIORef)
      _ <- hsIORef.mod(h :: _)
    } yield finalizerHandle[RegionT[S, P, ?]](h)).liftIO[P]))

  /**
   * Execute a region inside its parent region P. All resources which have been opened in the given
   * region and which haven't been duplicated using "dup", will be closed on exit from this function
   * whether by normal termination or by raising an exception.
   * Also all resources which have been duplicated to this region from a child region are closed
   * on exit if they haven't been duplicated themselves.
   * The Forall quantifier prevents resources from being returned by this function.
   */
  def runRegionT[P[_] : MonadControlIO, A](r: Forall[RegionT[?, P, A]]): P[A] = {
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

  def tailrecM[A, B](f: A => IO[A \/ B])(a: A): IO[B] =
    io(rw =>
      BindRec[Trampoline].tailrecM[(Tower[IvoryTower], A), (Tower[IvoryTower], B)] {
        case (nw0, x) =>
          f(x)(nw0).map {
            case (nw1, e) =>
              e.bimap((nw1, _), (nw1, _))
          }
      }((rw, a))
    )

  /** An IO action is an ST action. */
  implicit def IOToST[A](io: IO[A]): ST[IvoryTower, A] =
    st(io(_).run)

  /** An IO action that does nothing. */
  val ioUnit: IO[Unit] =
    IO(())
}
