package scalaz

trait CoPointed[F[_]] {
  def coPoint[A]: F[A] => A
}

object CoPointed extends CoPointeds

trait CoPointeds {
  implicit def ZeroCoPointed: CoPointed[Zero] = new CoPointed[Zero] {
    def coPoint[A] = a => a.zero
  }

  implicit def Tuple1CoPointed: CoPointed[Tuple1] = new CoPointed[Tuple1] {
    def coPoint[A] = a => a._1
  }

  implicit def Tuple2CoPointed[R]: CoPointed[({type λ[α]=(R, α)})#λ] = new CoPointed[({type λ[α]=(R, α)})#λ] {
    def coPoint[A] = a => a._2
  }

  implicit def Function0CoPointed: CoPointed[Function0] = new CoPointed[Function0] {
    def coPoint[A] = a => a.apply
  }

  import java.util.concurrent.Callable

  implicit def CallableCoPointed: CoPointed[Callable] = new CoPointed[Callable] {
    def coPoint[A] = a => a.call
  }

  import java.util.Map.Entry

  implicit def MapEntryCoPointed[X]: CoPointed[({type λ[α]=Entry[X, α]})#λ] = new CoPointed[({type λ[α]=Entry[X, α]})#λ] {
    def coPoint[A] = a => a.getValue
  }

  implicit def TreeLocCoPointed: CoPointed[TreeLoc] = new CoPointed[TreeLoc] {
    def coPoint[A] = a => a.tree.rootLabel
  }

  implicit def IdentityCoPointed: CoPointed[Identity] = new CoPointed[Identity] {
    def coPoint[A] = a => a.value
  }

  implicit def CoStateCoPointed[A, F[_] : CoPointed]: CoPointed[({type λ[α] = CoStateT[A, F, α]})#λ] = new CoPointed[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def coPoint[X] =
      _.copointT
  }

}
