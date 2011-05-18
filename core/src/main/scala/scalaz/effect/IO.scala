package scalaz
package effect

import RealWorld._
import RegionT._
import RefCountedFinalizer._
import FinalizerHandle._
import ST._
import data.Kleisli._

sealed trait IO[A] {
  private[effect] def apply(rw: World[RealWorld]): (World[RealWorld], A)

  import IO._

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

  def liftIO[M[_]](implicit m: MonadIO[M]): M[A] =
    m.liftIO.liftIO(this)

  /**Executes the handler if an exception is raised. */
  def except(handler: Throwable => IO[A]): IO[A] =
    IO(rw => try {
      this(rw)
    } catch {
      case e => handler(e)(rw)
    })

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
   * Returns an Either result which is Right if no exception was raised, or Left if an
   * exception was raised.
   */
  def catchLeft: IO[Either[Throwable, A]] = map(Right(_): Either[Throwable, A]) except (t => IOPointed.point(Left(t): Either[Throwable, A]))

  /**Like "catchLeft" but takes a predicate to select which exceptions are caught. */
  def catchSomeLeft[B](p: Throwable => Option[B]): IO[Either[B, A]] = for {
    r <- this.catchLeft
    x <- r match {
      case Right(v) => IOPointed.point(Right(v): Either[B, A])
      case Left(e) => p(e) match {
        case Some(b) => IOPointed.point(Left(b): Either[B, A])
        case None => throw e
      }
    }
  } yield x

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
}

object IO extends IOs

trait IOs {
  type RunInBase[M[_], Base[_]] =
  Forall[({type λ[B] = M[B] => Base[M[B]]})#λ]

  def apply[A](f: World[RealWorld] => (World[RealWorld], A)): IO[A] = new IO[A] {
    private[effect] def apply(rw: World[RealWorld]) = f(rw)
  }

  // Standard I/O
  def getChar: IO[Char] = IO(rw => (rw, readChar))

  def putChar(c: Char): IO[Unit] = IO(rw => (rw, {
    print(c);
    ()
  }))

  def putStr(s: String): IO[Unit] = IO(rw => (rw, {
    print(s);
    ()
  }))

  def putStrLn(s: String): IO[Unit] = IO((rw => (rw, {
    println(s);
    ()
  })))

  def readLn: IO[String] = IO(rw => (rw, readLine))

  def putOut[A](a: A): IO[Unit] = IO(rw => (rw, {
    print(a);
    ()
  }))

  // Mutable variables in the IO monad
  def newIORef[A](a: => A): IO[IORef[A]] =
    STToIO(newVar(a)) flatMap (v => IO.IOPointed.point(IORef.ioRef(v)))

  /**Throw the given error in the IO monad. */
  def throwIO[A](e: Throwable): IO[A] = IO(rw => (rw, throw e))

  def idLiftControl[M[_], A](f: RunInBase[M, M] => M[A])(implicit m: Monad[M]): M[A] =
    f(new RunInBase[M, M] {
      def apply[B] = (x: M[B]) => m.point(x)
    })

  def controlIO[M[_], A](f: RunInBase[M, IO] => IO[M[A]])(implicit m: MonadControlIO[M]): M[A] =
    m.monad.jn(m.liftControlIO.liftControlIO(f))

  /**
   * Register a finalizer in the current region. When the region terminates,
   * all registered finalizers will be performed if they're not duplicated to a parent region.
   */
  def onExit[S, P[_] : MonadIO](finalizer: IO[Unit]):
  RegionT[S, P, FinalizerHandle[({type λ[α] = RegionT[S, P, α]})#λ]] =
    regionT(kleisli(hsIORef => (for {
      refCntIORef <- newIORef(1)
      val h = refCountedFinalizer(finalizer)(refCntIORef)
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
  def runRegionT[P[_] : MonadControlIO, A](r: Forall[({type λ[S] = RegionT[S, P, A]})#λ]): P[A] = {
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

  implicit def IOToST[A](io: IO[A]): ST[RealWorld, A] =
    st(io(_))

  implicit def IOMonoid[A: Monoid]: Monoid[IO[A]] =
    Monoid.liftMonoid

  val ioUnit: IO[Unit] =
    IO(rw => (rw, ()))

  implicit val IOFunctor: Functor[IO] = new Functor[IO] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit val IOPointed: Pointed[IO] = new Pointed[IO] {
    def point[A](a: => A) = IO(rw => (rw, a))
  }

  implicit val IOPointedFunctor: PointedFunctor[IO] =
    PointedFunctor.pointedFunctor[IO]

  implicit val IOBind: Bind[IO] = new Bind[IO] {
    def bind[A, B](f: A => IO[B]) =
      _ flatMap f
  }

  implicit val IOJoin: Join[IO] = new Join[IO] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit val IOApplic: Applic[IO] = new Applic[IO] {
    def applic[A, B](f: IO[A => B]) =
      a =>
        for {
          ff <- f
          aa <- a
        } yield ff(aa)
  }

  implicit val IOPointedApplicative: Applicative[IO] =
    Applicative.applicative[IO]

  implicit val IOMonad: Monad[IO] =
    Monad.monadBP[IO]
}

