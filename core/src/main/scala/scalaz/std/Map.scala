package scalaz
package std

trait MapInstances {
  import syntax.std.function2V._
  
  implicit def mapMonoid[K, V: Semigroup]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K, V]()
    def append(m1: Map[K, V], m2: => Map[K, V]) = {
      // semigroups are not commutative, so order may matter. 
      val (from, to, semigroup) = {
        if (m1.size > m2.size) (m2, m1, Semigroup[V].append(_: V, _: V))
        else (m1, m2, (Semigroup[V].append(_: V, _: V)).flip)
      }

      from.foldLeft(to) {
        case (to, (k, v)) => to + (k -> to.get(k).map(semigroup(_, v)).getOrElse(v))
      }
    }
  }
}

object map extends MapInstances
