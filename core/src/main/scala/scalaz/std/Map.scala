package scalaz
package std

trait MapInstances {
  import syntax.std.function2._
  
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
  implicit def mapEqual[K: Order, V: Equal]: Equal[Map[K, V]] = new Equal[Map[K, V]] {
    def equal(a1: Map[K, V], a2: Map[K, V]): Boolean = {
      import set._
      if (equalIsNatural) a1 == a2
      else Equal[Set[K]].equal(a1.keySet, a1.keySet) && {
        a1.forall {
          case (k, v) => Equal[V].equal(v, a2(k))
        }
      }
    }
    override val equalIsNatural: Boolean = Equal[K].equalIsNatural && Equal[V].equalIsNatural
  }
}

object map extends MapInstances
