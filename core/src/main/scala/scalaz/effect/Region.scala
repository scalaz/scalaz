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
case class Region[S, P[_]:Monad, A](value: Kleisli[P, IORef[List[RefCountedFinalizer]], A])
  extends NewType[Kleisli[P, IORef[List[RefCountedFinalizer]], A]] {
    def map[B](f: A => B): Region[S, P, B] = Region(value map f)
    def flatMap[B](f: A => Region[S, P, B]): Region[S, P, B] =
      Region.regionBind.bind(this, f)
  }

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
  def dup[PP[_]:MonadIO:Monad, CS, PS]:
    H[({type λ[α] = Region[CS, ({type λ[β] = Region[PS, PP, β]})#λ, α]})#λ] =>
    Region[CS, ({type λ[α] = Region[PS, PP, α]})#λ, H[({type λ[β] = Region[PS, PP, β]})#λ]]
}

object Dup {
  import Scalaz._
  implicit val finalizerHandleDup: Dup[FinalizerHandle] = new Dup[FinalizerHandle] {
    def dup[PP[_]: MonadIO: Monad, CS, PS]:
      FinalizerHandle[({type λ[α] = Region[CS, ({type λ[β] = Region[PS, PP, β]})#λ, α]})#λ] =>
      Region[CS, ({type λ[α] = Region[PS, PP, α]})#λ, FinalizerHandle[({type λ[β] = Region[PS, PP, β]})#λ]] = h => 
        Region[CS, ({type λ[α] = Region[PS, PP, α]})#λ, FinalizerHandle[({type λ[β] = Region[PS, PP, β]})#λ]](
          kleisli[({type λ[α] = Region[PS, PP, α]})#λ,
                  IORef[List[RefCountedFinalizer]],
                  FinalizerHandle[({type λ[β] = Region[PS, PP, β]})#λ]](hsIORef =>
                    copy[PS, PP, ({type λ[α] = Region[CS, ({type λ[β] = Region[PS, PP, β]})#λ, α]})#λ](h)))
  }

  def copy[S, P[_]: MonadIO: Monad, R[_]](h: FinalizerHandle[R]):
    Region[S, P, FinalizerHandle[({type λ[α] = Region[S, P, α]})#λ]] = h match {
      case FinalizerHandle(h@RefCountedFinalizer(_, refCntIORef)) => 
        Region(kleisli(hsIORef => (for {
          _ <- refCntIORef.mod(_ + 1)
          _ <- hsIORef.mod(h :: _)
        } yield FinalizerHandle[({type λ[α] = Region[S, P, α]})#λ](h)).liftIO[P]))
    }
}

sealed trait AncestorRegion[P[_], C[_]]
sealed trait RootRegion[A]
trait LocalRegion[SL, S, A]
trait Local[S]

object Region {
  import Scalaz._

  implicit def regionBind[S, M[_]:Monad]: Bind[({type λ[α] = Region[S, M, α]})#λ] =
    new Bind[({type λ[α] = Region[S, M, α]})#λ] {
      def bind[A, B](m: Region[S, M, A], f: A => Region[S, M, B]): Region[S, M, B] = 
        Region(kleisli(s => m.value(s) >>= (a => f(a).value(s))))
    }

  implicit def regionPure[S, M[_]:Monad]: Pure[({type λ[α] = Region[S, M, α]})#λ] =
    new Pure[({type λ[α] = Region[S, M, α]})#λ] {
      def pure[A](a: => A): Region[S, M, A] = Region(kleisli(s => a.pure[M]))
   }

  implicit def regionMonad[S, M[_]:Monad]: Monad[({type λ[α] = Region[S, M, α]})#λ] =
    Monad.monad[({type λ[α] = Region[S, M, α]})#λ](regionBind, regionPure)

  implicit def regionFunctor[S, M[_]:Monad]: Functor[({type λ[α] = Region[S, M, α]})#λ] =
    new Functor[({type λ[α] = Region[S, M, α]})#λ] {
      def fmap[A, B](m: Region[S, M, A], f: A => B): Region[S, M, B] =
        Region(kleisli(s => m.value(s) map f))
    }

  implicit def reflexivity[S, M[_]]: AncestorRegion[({type λ[α] = Region[S, M, α]})#λ, ({type λ[α] = Region[S, M, α]})#λ] =
    new AncestorRegion[({type λ[α] = Region[S, M, α]})#λ, ({type λ[α] = Region[S, M, α]})#λ] {}

  implicit def transitivity[S, P[_], C[_]](implicit witness: AncestorRegion[P, C]):
    AncestorRegion[P, ({type λ[α] = Region[S, C, α]})#λ] = new AncestorRegion[P, ({type λ[α] = Region[S, C, α]})#λ] {}

  implicit def initiality[S, M[_]]: AncestorRegion[RootRegion, ({type λ[α] = Region[S, M, α]})#λ] =
    new AncestorRegion[RootRegion, ({type λ[α] = Region[S, M, α]})#λ] {}

  implicit def localAncestor[SF, S, M[_]]:
    AncestorRegion[({type λ[α] = LocalRegion[SF, S, α]})#λ, ({type λ[α] = Region[Local[S], M, α]})#λ] =
      new AncestorRegion[({type λ[α] = LocalRegion[SF, S, α]})#λ, ({type λ[α] = Region[Local[S], M, α]})#λ] {}

  implicit def localToRegion[S, M[_]]: AncestorRegion[({type λ[α] = Region[S, M, α]})#λ, ({type λ[α] = Region[Local[S], M, α]})#λ] =
    new AncestorRegion[({type λ[α] = Region[S, M, α]})#λ, ({type λ[α] = Region[Local[S], M, α]})#λ] {}

  implicit def regionToLocal[S, M[_]]: AncestorRegion[({type λ[α] = Region[Local[S], M, α]})#λ, ({type λ[α] = Region[S, M, α]})#λ] =
    new AncestorRegion[({type λ[α] = Region[Local[S], M, α]})#λ, ({type λ[α] = Region[S, M, α]})#λ] {}

}
