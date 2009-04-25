package scalaz.memo

sealed trait Comemo[-T, K, V] {
  def apply(t: T): Memo[K, V]
}

object Comemo {
  def comemo[T, K, V](f: T => Memo[K, V]) = new Comemo[T, K, V] {
    def apply(t: T) = f(t)
  }

  implicit def ComemoCofunctor[K, V] = new Cofunctor[PartialApply2Of3[Comemo, K, V]#ApplyA] {
    def comap[A, B](r: Comemo[A, K, V], f: B => A) = comemo[B, K, V](b => r(f(b)))   
  }
}