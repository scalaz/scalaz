package scalaz
package effect

/**Duplicate a handle in the parent region. */
trait Dup[H[_[_]]] {
  def dup[PP[_] : MonadIO, CS, PS]:
  H[({type λ[α] = RegionT[CS, ({type λ[β] = RegionT[PS, PP, β]})#λ, α]})#λ] =>
      RegionT[CS, ({type λ[α] = RegionT[PS, PP, α]})#λ, H[({type λ[β] = RegionT[PS, PP, β]})#λ]]
}

object Dup extends DupInstances with DupFunctions

sealed abstract class DupInstances {
  import Dup._

  implicit val FinalizerHandleDup: Dup[FinalizerHandle] = new Dup[FinalizerHandle] {
    def dup[PP[_] : MonadIO, CS, PS]:
    FinalizerHandle[({type λ[α] = RegionT[CS, ({type λ[β] = RegionT[PS, PP, β]})#λ, α]})#λ] =>
        RegionT[CS, ({type λ[α] = RegionT[PS, PP, α]})#λ, FinalizerHandle[({type λ[β] = RegionT[PS, PP, β]})#λ]] = h =>
      RegionT[CS, ({type λ[α] = RegionT[PS, PP, α]})#λ, FinalizerHandle[({type λ[β] = RegionT[PS, PP, β]})#λ]](
        Kleisli[({type λ[α] = RegionT[PS, PP, α]})#λ, IORef[List[RefCountedFinalizer]],
            FinalizerHandle[({type λ[β] = RegionT[PS, PP, β]})#λ]](hsIORef =>
          copy[PS, PP, ({type λ[α] = RegionT[CS, ({type λ[β] = RegionT[PS, PP, β]})#λ, α]})#λ](h)))
  }
}

trait DupFunctions {

  /**Duplicates a handle to its parent region. */
  def dup[H[_[_]] : Dup, PP[_] : MonadIO, CS, PS](h: H[({type λ[α] = RegionT[CS, ({type λ[β] = RegionT[PS, PP, β]})#λ, α]})#λ]):
  RegionT[CS, ({type λ[α] = RegionT[PS, PP, α]})#λ, H[({type λ[β] = RegionT[PS, PP, β]})#λ]] = implicitly[Dup[H]].dup.apply(h)

  def copy[S, P[_] : MonadIO, R[_]](h: FinalizerHandle[R]):
  RegionT[S, P, FinalizerHandle[({type λ[α] = RegionT[S, P, α]})#λ]] = h match {
    case h =>
      RegionT(Kleisli(hsIORef => (for {
        _ <- h.finalizer.refcount.mod(_ + 1)
        _ <- hsIORef.mod(h.finalizer :: _)
      } yield FinalizerHandle[({type λ[α] = RegionT[S, P, α]})#λ](h.finalizer)).liftIO[P]))
  }
}
