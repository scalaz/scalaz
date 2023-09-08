package scalaz
package std

import scala.collection.immutable.SortedMap

trait SortedMapInstances0 {
  import syntax.std.function2._
  private[std] sealed trait SortedMapMonoid[K, V] extends Monoid[SortedMap[K, V]] {
    implicit def V: Semigroup[V]
    implicit def O: scala.Ordering[K]

    def zero = SortedMap.empty[K, V]
    def append(m1: SortedMap[K, V], m2: => SortedMap[K, V]) = {
      // Eagerly consume m2 as the value is used more than once.
      val m2Instance: SortedMap[K, V] = m2
      // semigroups are not commutative, so order may matter.
      val (from, to, semigroup) = {
        if (m1.size > m2Instance.size) (m2Instance, m1, Semigroup[V].append(_: V, _: V))
        else (m1, m2Instance, (Semigroup[V].append(_: V, _: V)).flip)
      }

      from.foldLeft(to) {
        case (to, (k, v)) => to + ((k, to.get(k).map(semigroup(_, v)).getOrElse(v)))
      }
    }
  }

  private[std] trait SortedMapEqual[K, V] extends Equal[SortedMap[K, V]] {
    implicit def OK: Order[K]
    implicit def OV: Equal[V]

    override def equal(a1: SortedMap[K, V], a2: SortedMap[K, V]): Boolean = {
      import set._
      if (equalIsNatural) a1 == a2
      else Equal[Set[K]].equal(a1.keySet.toSet, a2.keySet.toSet) && {
        a1.forall {
          case (k, v) => a2.get(k).exists(v2 => Equal[V].equal(v, v2))
        }
      }
    }
    override val equalIsNatural: Boolean = Equal[K].equalIsNatural && Equal[V].equalIsNatural
  }

  private[std] trait SortedMapFoldable[K] extends Foldable.FromFoldr[SortedMap[K, *]] {
    override def foldLeft[A, B](fa: SortedMap[K, A], z: B)(f: (B, A) => B) =
      fa.valuesIterator.foldLeft(z)(f)

    override def foldRight[A, B](fa: SortedMap[K, A], z: => B)(f: (A, => B) => B) =
      fa.foldRight(z)((p, b) => f(p._2, b))

    override final def all[A](fa: SortedMap[K, A])(f: A => Boolean) =
      fa.valuesIterator.forall(f)

    override final def any[A](fa: SortedMap[K, A])(f: A => Boolean) =
      fa.valuesIterator.exists(f)
  }

  implicit def sortedMapEqual[K: Order, V: Equal]: Equal[SortedMap[K, V]] =
    new SortedMapEqual[K, V] {
      def OK = Order[K]
      def OV = Equal[V]
    }

  implicit def sortedMapFoldable[K]: Foldable[SortedMap[K, *]] =
    new SortedMapFoldable[K]{}

  implicit def sortedMapBand[K, V](implicit S: Band[V], K: scala.Ordering[K]): Band[SortedMap[K, V]] =
    new SortedMapMonoid[K, V] with Band[SortedMap[K, V]] {
      override def V: Band[V] = S
      override def O = K
    }
}

trait SortedMapInstances extends SortedMapInstances0 with SortedMapFunctions {
  import Liskov.<~<

  /** Covariant over the value parameter, where `plus` applies the
    * `Last` semigroup to values.
    */
  implicit def sortedMapInstance[K: scala.Ordering]: Traverse[SortedMap[K, *]] with IsEmpty[SortedMap[K, *]] with Bind[SortedMap[K, *]] with Align[SortedMap[K, *]] =
    new Traverse[SortedMap[K, *]] with IsEmpty[SortedMap[K, *]] with Bind[SortedMap[K, *]] with SortedMapFoldable[K] with Align[SortedMap[K, *]] {
      def empty[V] = SortedMap.empty[K, V]
      def plus[V](a: SortedMap[K, V], b: => SortedMap[K, V]) = a ++ b
      def isEmpty[V](fa: SortedMap[K, V]) = fa.isEmpty
      def bind[A, B](fa: SortedMap[K,A])(f: A => SortedMap[K, B]) = fa.collect{case (k, v) if f(v).isDefinedAt(k) => k -> f(v)(k)}
      override def map[A, B](fa: SortedMap[K, A])(f: A => B) = fa.map{case (k, v) => (k, f(v))}
      override def widen[A, B](fa: SortedMap[K, A])(implicit ev: A <~< B) = Liskov.co[SortedMap[K, +*], A, B](ev)(fa)
      def traverseImpl[G[_],A,B](m: SortedMap[K,A])(f: A => G[B])(implicit G: Applicative[G]): G[SortedMap[K,B]] =
        G.map(list.listInstance.traverseImpl(m.toList)({ case (k, v) => G.map(f(v))(k -> _) }))(xs => SortedMap(xs:_*))
      import \&/._
      override def alignWith[A, B, C](f: A \&/ B => C) = {
        case (a, b) if b.isEmpty => map(a)(v => f(This(v)))
        case (a, b) if a.isEmpty => map(b)(v => f(That(v)))
        case (a, b) =>
          map(unionWith(map(a)(This(_): A \&/ B), map(b)(That(_): A \&/ B)){
            case (This(aa), That(bb)) => Both(aa, bb)
            case _ => sys.error("SortedMap alignWith")
          })(f)
      }
      override def align[A, B](a: SortedMap[K, A], b: SortedMap[K, B]) = (a, b) match {
        case (a, b) if b.isEmpty => map(a)(This(_))
        case (a, b) if a.isEmpty => map(b)(That(_))
        case (a, b) =>
          unionWith(map(a)(This(_): A \&/ B), map(b)(That(_): A \&/ B)){
            case (This(aa), That(bb)) => Both(aa, bb)
            case _ => sys.error("SortedMap align")
          }
      }
    }

  /** SortedMap union monoid, unifying values with `V`'s `append`. */
  implicit def sortedMapMonoid[K, V](implicit S: Semigroup[V], K: scala.Ordering[K]): Monoid[SortedMap[K, V]] =
    new SortedMapMonoid[K, V] {
      override def V = S
      override def O = K
    }

  implicit def sortedMapShow[K, V](implicit K: Show[K], V: Show[V]): Show[SortedMap[K, V]] =
    Show.show(m => "SortedMap[" +:
                Cord.mkCord(", ", m.toSeq.map{
                  case (k, v) => Cord(K show k, "->", V show v)
                }: _*) :+ "]")

  implicit def sortedMapOrder[K: Order, V: Order]: Order[SortedMap[K, V]] =
    new Order[SortedMap[K, V]] with SortedMapEqual[K, V] {
      override def OK: Order[K] = Order[K]
      override def OV: Equal[V] = Equal[V]
      def order(x: SortedMap[K, V], y: SortedMap[K, V]): Ordering = {
        import vector._
        import anyVal._
        import tuple._
        implicit val ok: scala.Ordering[K] = Order[K].toScalaOrdering
        Semigroup[Ordering]
         .append(Order[Int].order(x.size, y.size),
                 Order[Vector[(K, V)]]
                  .order(x.toVector.sortBy((_:(K,V))._1),
                         y.toVector.sortBy((_:(K,V))._1)))
      }
    }
}

trait SortedMapFunctions {
  /** Vary the value of `m get k`. */
  final def alter[K, A](m: SortedMap[K, A], k: K)(f: (Option[A] => Option[A])): SortedMap[K, A] =
    f(m get k) map (x => m + ((k, x))) getOrElse (m - k)

  /** Like `intersectWith`, but tell `f` about the key. */
  final def intersectWithKey[K: scala.Ordering, A, B, C](m1: SortedMap[K, A], m2: SortedMap[K, B])(f: (K, A, B) => C): SortedMap[K, C] = m1 collect {
    case (k, v) if m2 contains k => k -> f(k, v, m2(k))
  }

  /** Collect only elements with matching keys, joining their
    * associated values with `f`.
    */
  final def intersectWith[K: scala.Ordering, A, B, C](m1: SortedMap[K, A], m2: SortedMap[K, B])(f: (A, B) => C): SortedMap[K, C] =
    intersectWithKey(m1, m2)((_, x, y) => f(x, y))

  /** Exchange keys of `m` according to `f`.  Result may be smaller if
    * `f` maps two or more `K`s to the same `K2`, in which case the
    * resulting associated value is an arbitrary choice.
    */
  final def mapKeys[K, K2: scala.Ordering, A](m: SortedMap[K, A])(f: K => K2): SortedMap[K2, A] =
    m map {case (k, v) => f(k) -> v}

  /** Like `unionWith`, but telling `f` about the key. */
  final def unionWithKey[K: scala.Ordering, A](m1: SortedMap[K, A], m2: SortedMap[K, A])(f: (K, A, A) => A): SortedMap[K, A] = {
    val diff = m2 -- m1.keySet
    val aug = m1.map {
      case (k, v) => (k, if (m2 contains k) f(k, v, m2(k)) else v)
    }
    aug ++ diff
  }

  /** Union, resolving collisions with `f`, where the first arg is
    * guaranteed to be from `m1`, the second from `m2`.
    *
    * @note iff `f` gives rise to a [[scalaz.Semigroup]], so does
    *       `unionWith(_, _)(f)`.*/
  final def unionWith[K: scala.Ordering, A](m1: SortedMap[K, A], m2: SortedMap[K, A])(f: (A, A) => A): SortedMap[K, A] =
    unionWithKey(m1, m2)((_, x, y) => f(x, y))

  /** As with `SortedMap.updated`, but resolve a collision with `f`.  The
    * first argument is guaranteed to be from `m1`.
    */
  final def insertWith[K, A](m1: SortedMap[K, A], k: K, v: A)(f: (A, A) => A): SortedMap[K, A] =
    if(m1 contains k) m1 + ((k, f(m1(k), v))) else m1 + ((k, v))

  /** Grab a value out of SortedMap if it's present. Otherwise evaluate
    * a value to be placed at that key in the SortedMap.
    */
  final def getOrAdd[F[_], K, A](m: SortedMap[K, A], k: K)(fa: => F[A])(implicit F: Applicative[F]): F[(SortedMap[K, A], A)] =
    (m get k).fold(F.map(fa)(a => (m + ((k, a)), a)))(a => F.point((m, a)))
}

object sortedMap extends SortedMapInstances with SortedMapFunctions
