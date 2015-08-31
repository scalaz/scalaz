package scalaz
package effect

/**Duplicate a handle in the parent region. */
trait Dup[H[_[_]]] {
  def dup[PP[_]: MonadIO, CS, PS]: H[RegionT[CS, RegionT[PS, PP, ?], ?]] => RegionT[CS, RegionT[PS, PP, ?], H[RegionT[PS, PP, ?]]]
}

sealed abstract class DupInstances {
  import Dup._

  implicit val FinalizerHandleDup: Dup[FinalizerHandle] =
    new Dup[FinalizerHandle] {
      def dup[PP[_] : MonadIO, CS, PS]: FinalizerHandle[RegionT[CS, RegionT[PS, PP, ?], ?]] => RegionT[CS, RegionT[PS, PP, ?], FinalizerHandle[RegionT[PS, PP, ?]]] = h =>
        RegionT[CS, RegionT[PS, PP, ?], FinalizerHandle[RegionT[PS, PP, ?]]](
          Kleisli[RegionT[PS, PP, ?], IORef[List[RefCountedFinalizer]],
              FinalizerHandle[RegionT[PS, PP, ?]]](hsIORef =>
            copy[PS, PP, RegionT[CS, RegionT[PS, PP, ?], ?]](h)))
    }
}

object Dup extends DupInstances {

  /**Duplicates a handle to its parent region. */
  def dup[H[_[_]] : Dup, PP[_] : MonadIO, CS, PS](h: H[RegionT[CS, RegionT[PS, PP, ?], ?]]):
  RegionT[CS, RegionT[PS, PP, ?], H[RegionT[PS, PP, ?]]] = implicitly[Dup[H]].dup.apply(h)

  def copy[S, P[_] : MonadIO, R[_]](h: FinalizerHandle[R]):
  RegionT[S, P, FinalizerHandle[RegionT[S, P, ?]]] = h match {
    case h =>
      RegionT(Kleisli(hsIORef => (for {
        _ <- h.finalizer.refcount.mod(_ + 1)
        _ <- hsIORef.mod(h.finalizer :: _)
      } yield FinalizerHandle[RegionT[S, P, ?]](h.finalizer)).liftIO[P]))
  }
}
