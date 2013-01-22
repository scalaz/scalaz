package scalaz
package std

trait MapInstances0 {
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

trait MapInstances extends MapInstances0 {
  import syntax.std.function2._

  /** Covariant over the value parameter, where `plus` applies the
    * `Last` semigroup to values.
    */
  implicit def mapInstance[K] = new Traverse[({type F[V] = Map[K,V]})#F] with IsEmpty[({type F[V] = Map[K,V]})#F] {
    def empty[V] = Map.empty[K, V]
    def plus[V](a: Map[K, V], b: => Map[K, V]) = a ++ b
    def isEmpty[V](fa: Map[K, V]) = fa.isEmpty

    def traverseImpl[G[_],A,B](m: Map[K,A])(f: A => G[B])(implicit G: Applicative[G]): G[Map[K,B]] = {
      import G.functorSyntax._
      list.listInstance.traverseImpl(m.toList)({ case (k, v) => f(v) map (k -> _) }) map (_.toMap)
    }
  }

  /** Map union monoid, unifying values with `V`'s `append`. */
  implicit def mapMonoid[K, V: Semigroup]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K, V]()
    def append(m1: Map[K, V], m2: => Map[K, V]) = {
      // Eagerly consume m2 as the value is used more than once.
      val m2Instance: Map[K, V] = m2
      // semigroups are not commutative, so order may matter.
      val (from, to, semigroup) = {
        if (m1.size > m2Instance.size) (m2Instance, m1, Semigroup[V].append(_: V, _: V))
        else (m1, m2Instance, (Semigroup[V].append(_: V, _: V)).flip)
      }

      from.foldLeft(to) {
        case (to, (k, v)) => to + (k -> to.get(k).map(semigroup(_, v)).getOrElse(v))
      }
    }
  }

  implicit def mapShow[K, V](implicit K: Show[K], V: Show[V]): Show[Map[K, V]] =
    Show.show(m => "Map[" +:
                Cord.mkCord(", ", m.toSeq.view.map{
                  case (k, v) => Cord(K show k, "->", V show v)
                }: _*) :+ "]")

  implicit def mapOrder[K: Order, V: Order]: Order[Map[K, V]] = new Order[Map[K, V]] {
    def order(x: Map[K, V], y: Map[K, V]): Ordering = {
      import list._
      import tuple._
      implicit val ok = implicitly[Order[K]].toScalaOrdering
      Order[List[(K, V)]].order(x.toList.sortBy(_._1), y.toList.sortBy(_._1))
    }
  }
}

trait MapFunctions {
  /** Vary the value of `m get k`. */
  final def alter[K, A](m: Map[K, A], k: K)(f: (Option[A] => Option[A])): Map[K, A] =
    f(m get k) map (m.updated(k, _)) getOrElse (m - k)

  /** Like `intersectWith`, but tell `f` about the key. */
  final def intersectWithKey[K,A,B,C](m1: Map[K, A], m2: Map[K, B])(f: (K, A, B) => C): Map[K, C] = m1 collect {
    case (k, v) if m2 contains k => k -> f(k, v, m2(k))
  }

  /** Collect only elements with matching keys, joining their
    * associated values with `f`.
    */
  final def intersectWith[K,A,B,C](m1: Map[K, A], m2: Map[K, B])(f: (A, B) => C): Map[K, C] =
    intersectWithKey(m1, m2)((_, x, y) => f(x, y))

  /** Exchange keys of `m` according to `f`.  Result may be smaller if
    * `f` maps two or more `K`s to the same `K2`, in which case the
    * resulting associated value is an arbitrary choice.
    */
  final def mapKeys[K, K2, A](m: Map[K, A])(f: K => K2): Map[K2, A] =
    m map {case (k, v) => f(k) -> v}

  /** Like `unionWith`, but telling `f` about the key. */
  final def unionWithKey[K,A](m1: Map[K, A], m2: Map[K, A])(f: (K, A, A) => A): Map[K, A] = {
    val diff = m2 -- m1.keySet
    val aug = m1 map {
      case (k, v) => if (m2 contains k) k -> f(k, v, m2(k)) else (k, v)
    }
    aug ++ diff
  }

  /** Union, resolving collisions with `f`, where the first arg is
    * guaranteed to be from `m1`, the second from `m2`.
    *
    * @note iff `f` gives rise to a [[scalaz.Semigroup]], so does
    *       `unionWith(_, _)(f)`.*/
  final def unionWith[K,A](m1: Map[K, A], m2: Map[K, A])(f: (A, A) => A): Map[K, A] =
    unionWithKey(m1, m2)((_, x, y) => f(x, y))

  /** As with `Map.updated`, but resolve a collision with `f`.  The
    * first argument is guaranteed to be from `m1`.
    */
  final def insertWith[K,A](m1: Map[K, A], k: K, v: A)(f: (A, A) => A): Map[K, A] =
    if(m1 contains k) m1 + (k -> f(m1(k), v)) else m1 + (k -> v)
}

object map extends MapInstances with MapFunctions

