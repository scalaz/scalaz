package scalaz

import java.util.Map.Entry
import java.util.AbstractMap.SimpleImmutableEntry

trait CoJoin[M[_]] {
  def coJoin[A]: M[A] => M[M[A]]
}

object CoJoin extends CoJoins

trait CoJoins {
  implicit def MapEntryCojoin[X]: CoJoin[({type λ[α] = Entry[X, α]})#λ] = new CoJoin[({type λ[α] = Entry[X, α]})#λ] {
    def coJoin[A] = (a: Entry[X, A]) => new SimpleImmutableEntry(a.getKey, a)
  }
}
