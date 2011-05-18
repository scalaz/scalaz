package scalaz

import java.util.Map.Entry

trait CoPointed[F[_]] {
  def coPoint[A]: F[A] => A
}

object CoPointed extends CoPointeds

trait CoPointeds {
  implicit def MapEntryCoPointed[X]: CoPointed[({type λ[α] = Entry[X, α]})#λ] = new CoPointed[({type λ[α] = Entry[X, α]})#λ] {
    def coPoint[A] = (a: Entry[X, A]) => a.getValue
  }
}
