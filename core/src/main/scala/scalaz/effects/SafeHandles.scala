package scalaz
package effects

import Scalaz._

trait Close[H] {
  def close(h: H): IO[Unit]
}

class SafeHandles[Handle: Close] {
  /** IO Region Transformer. A region is identified by a label which is a fresh type variable. */
  case class IORT[S, M[_], V](value: Kleisli[M, IORef[List[HandleR]], V])
    extends NewType[Kleisli[M, IORef[List[HandleR]], V]] {
      def lift[S1]: IORT[S1, ({ type λ[α] = IORT[S, M, α]})#λ, V] =
        IORT[S1, ({ type λ[α] = IORT[S, M, α]})#λ, V](
          kleisli[({ type λ[α] = IORT[S, M, α]})#λ, IORef[List[HandleR]], V](handles => this))
    }

  def hClose(h: Handle) = implicitly[Close[Handle]].close(h)

  type SIO[S, A] = IORT[S, IO, A]

  /** Tracks the duplication of handles using reference counting. */
  case class HandleR(handle: Handle, refcount: IORef[Int]) {
    def close: IO[Unit] = for {
      rc <- refcount.read
      _ <- if (rc > 0) (().pure[IO]) else error("Closed a handle too many times!")
      _ <- refcount.write(rc - 1)
      _ <- if (rc > 1) (().pure[IO]) else hClose(handle)
    } yield ()
  }

  def newHR(h: Handle): IO[HandleR] = newIORef(1) map (HandleR(h, _))
}
