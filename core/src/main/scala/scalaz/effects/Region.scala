package scalaz
package effects

// An implementation of "Lightweight Monadic Regions" by Kiselyov and Shan
// http://okmij.org/ftp/Haskell/regions.html#light-weight
// Based on a Haskell library by Bas van Dijk

/**
 * A monad transformer in which scarce resources can be opened. When the region
 * terminates, all opened resources will be closed automatically. It's a type error
 * to return an opened resorce from the region, and no I/O with closed
 * resources is possible.
 */
case class RegionT[S, P[_], A](value: Kleisli[P, IORef[List[RefCountedFinalizer]], A])
  extends NewType[Kleisli[P, IORef[List[RefCountedFinalizer]], A]]

/**
 * A finalizer paired with its reference count which defines how many times
 * it has been registered in some region.
 */
case class RefCountedFinalizer(finalizer: IO[Unit], refcount: IORef[Int])

/**
 * A handle to a finalizer that allows you to duplicate it to a parent region using "dup".
 * Duplicating a finalizer means that instead of being performed when the current region
 * terminates, it is performed when the parent region terminates.
 */
case class FinalizerHandle[R[_]](finalizer: RefCountedFinalizer)

/** Duplicate a handle in the parent region. */
trait Dup[H[_[_]]] {
  def dup[PP[_]:MonadIO, CS, PS]:
    H[({type λ[α] = RegionT[CS, ({type λ[β] = RegionT[PS, PP, β]})#λ, α]})#λ] =>
    RegionT[CS, ({type λ[α] = RegionT[PS, PP, α]})#λ, H[({type λ[β] = RegionT[PS, PP, β]})#λ]]
}

object Dup {
  import Scalaz._
  implicit val finalizerHandleDup: Dup[FinalizerHandle] = new Dup[FinalizerHandle] {
    def dup[PP[_]: MonadIO, CS, PS]:
      FinalizerHandle[({type λ[α] = RegionT[CS, ({type λ[β] = RegionT[PS, PP, β]})#λ, α]})#λ] =>
      RegionT[CS, ({type λ[α] = RegionT[PS, PP, α]})#λ, FinalizerHandle[({type λ[β] = RegionT[PS, PP, β]})#λ]] = h => 
        RegionT[CS, ({type λ[α] = RegionT[PS, PP, α]})#λ, FinalizerHandle[({type λ[β] = RegionT[PS, PP, β]})#λ]](
          kleisli[({type λ[α] = RegionT[PS, PP, α]})#λ,
                  IORef[List[RefCountedFinalizer]],
                  FinalizerHandle[({type λ[β] = RegionT[PS, PP, β]})#λ]](hsIORef =>
                    copy[PS, PP, ({type λ[α] = RegionT[CS, ({type λ[β] = RegionT[PS, PP, β]})#λ, α]})#λ](h)))
  }

  def copy[S, P[_]: MonadIO, R[_]](h: FinalizerHandle[R]):
    RegionT[S, P, FinalizerHandle[({type λ[α] = RegionT[S, P, α]})#λ]] = h match {
      case FinalizerHandle(h@RefCountedFinalizer(_, refCntIORef)) => 
        RegionT(kleisli(hsIORef => (for {
          _ <- refCntIORef.mod(_ + 1)
          _ <- hsIORef.mod(h :: _)
        } yield FinalizerHandle[({type λ[α] = RegionT[S, P, α]})#λ](h)).liftIO[P]))
    }
}

object RegionT {
  import Scalaz._

  implicit def regionTBind[S, M[_]:Bind]: Bind[({type λ[α] = RegionT[S, M, α]})#λ] =
    new Bind[({type λ[α] = RegionT[S, M, α]})#λ] {
      def bind[A, B](m: RegionT[S, M, A], f: A => RegionT[S, M, B]): RegionT[S, M, B] = 
        RegionT(kleisli(s => m.value(s) >>= (a => f(a).value(s))))
    }

  implicit def regionTPure[S, M[_]:Pure]: Pure[({type λ[α] = RegionT[S, M, α]})#λ] =
    new Pure[({type λ[α] = RegionT[S, M, α]})#λ] {
      def pure[A](a: => A): RegionT[S, M, A] = RegionT(kleisli(s => a.pure[M]))
    }

  implicit def regionMonad[S, M[_]:Monad]: Monad[({type λ[α] = RegionT[S, M, α]})#λ] =
    Monad.monad[({type λ[α] = RegionT[S, M, α]})#λ](regionTBind, regionTPure)

  /**
   * Reguster a finalizer in the current region. When the region terminates,
   * all registered finalizers will be performed if they're not duplicated to a parent region.
   */
  def onExit[S, P[_]: MonadIO](finalizer: IO[Unit]):
    RegionT[S, P, FinalizerHandle[({type λ[α] = RegionT[S, P, α]})#λ]] =
      RegionT(kleisli(hsIORef => (for {
        refCntIORef <- newIORef(1)
        val h = RefCountedFinalizer(finalizer, refCntIORef)
        _ <- hsIORef.mod(h :: _)
      } yield FinalizerHandle[({type λ[α] = RegionT[S, P, α]})#λ](h)).liftIO[P]))


  /**
   * Execute a region inside its parent region P. All resources which have been opened in the given
   * region and which haven't been duplicated using "dup", will be closed on exit from this function
   * whether by normal termination or by raising an exception.
   * Also all resources which have been duplicated to this region from a child region are closed
   * on exit if they haven't been duplicated themselves.
   * The Forall quantifier prevents resources from being returned by this function.
   */
  def runRegionT[P[_]:MonadControlIO, A](r: Forall[({type λ[S] = RegionT[S, P, A]})#λ]): P[A] = {
    def after(hsIORef: IORef[List[RefCountedFinalizer]]) = for {
      hs <- hsIORef.read
      _ <- hs.traverse_ {
        case RefCountedFinalizer(finalizer, refCntIORef) => for {
          refCnt <- refCntIORef.mod(_ - 1)
          _ <- if (refCnt == 0) finalizer else ().pure[IO]
        } yield ()
      }
    } yield ()
    newIORef(List[RefCountedFinalizer]()).bracketIO(after)(s => r.apply.value(s))
  }

}
