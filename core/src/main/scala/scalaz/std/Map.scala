package scalaz
package std

import collection.immutable.{Map, MapLike} // Just so we're clear.
import collection.generic.CanBuildFrom

trait MapSub {
  type XMap[K, V] <: Map[K, V] with MapLike[K, V, XMap[K, V]]
  /** Evidence on key needed to construct new maps. */
  type BuildKeyConstraint[K]
  protected implicit def buildXMap[K, V, K2: BuildKeyConstraint, V2]
      : CanBuildFrom[XMap[K, V], (K2, V2), XMap[K2, V2]]

  /** How `MapLike#updated` might be typed in a sane world.  A world
    * that embraced higher kinds, instead of shunning them.
    */
  protected def ab_+[K: BuildKeyConstraint, V
                   ](m: XMap[K, V], k: K, v: V): XMap[K, V]

  /** As with `ab_+, but with `MapLike#-`. */
  protected def ab_-[K: BuildKeyConstraint, V
                   ](m: XMap[K, V], k: K): XMap[K, V]

  private[std]
  def fromSeq[K: BuildKeyConstraint, V](as: (K, V)*): XMap[K, V] =
    buildXMap[K, V, K, V].apply().++=(as).result()
}

sealed trait MapSubMap extends MapSub {
  type XMap[K, V] = Map[K, V]
  type BuildKeyConstraint[K] = DummyImplicit
  protected final def buildXMap[K, V, K2: BuildKeyConstraint, V2] = implicitly

  protected final def ab_+[K: BuildKeyConstraint, V
                         ](m: XMap[K, V], k: K, v: V): XMap[K, V] =
    m updated (k, v)
  protected final def ab_-[K: BuildKeyConstraint, V
                         ](m: XMap[K, V], k: K): XMap[K, V] =
    m - k
}

trait MapSubInstances0 extends MapSub {
  private[std] trait MapEqual[K, V] extends Equal[XMap[K, V]] {
    implicit def OK: Order[K]
    implicit def OV: Equal[V]

    override def equal(a1: XMap[K, V], a2: XMap[K, V]): Boolean = {
      import set._
      if (equalIsNatural) a1 == a2
      else Equal[Set[K]].equal(a1.keySet, a2.keySet) && {
        a1.forall {
          case (k, v) => a2.get(k).exists(v2 => Equal[V].equal(v, v2))
        }
      }
    }
    override val equalIsNatural: Boolean = Equal[K].equalIsNatural && Equal[V].equalIsNatural
  }

  private[std] trait MapFoldable[K] extends Foldable.FromFoldr[XMap[K, ?]] {
    override def foldLeft[A, B](fa: XMap[K, A], z: B)(f: (B, A) => B) =
      fa.valuesIterator.foldLeft(z)(f)

    override def foldRight[A, B](fa: XMap[K, A], z: => B)(f: (A, => B) => B) =
      fa.foldRight(z)((p, b) => f(p._2, b))

    override final def all[A](fa: XMap[K, A])(f: A => Boolean) =
      fa.valuesIterator.forall(f)

    override final def any[A](fa: XMap[K, A])(f: A => Boolean) =
      fa.valuesIterator.exists(f)
  }

  implicit def mapEqual[K: Order, V: Equal]: Equal[XMap[K, V]] =
    new MapEqual[K, V] {
      def OK = Order[K]
      def OV = Equal[V]
    }

  implicit def mapFoldable[K]: Foldable[XMap[K, ?]] =
    new MapFoldable[K]{}
}

trait MapSubInstances extends MapSubInstances0 with MapSubFunctions {
  import syntax.std.function2._

  /** Covariant over the value parameter, where `plus` applies the
    * `Last` semigroup to values.
    */
  implicit def mapInstance[K: BuildKeyConstraint]: Traverse[XMap[K, ?]] with IsEmpty[XMap[K, ?]] with Bind[XMap[K, ?]] with Align[XMap[K, ?]] =
    new Traverse[XMap[K, ?]] with IsEmpty[XMap[K, ?]] with Bind[XMap[K, ?]] with MapFoldable[K] with Align[XMap[K, ?]] {
      def empty[V] = fromSeq[K, V]()
      def plus[V](a: XMap[K, V], b: => XMap[K, V]) = a ++ b
      def isEmpty[V](fa: XMap[K, V]) = fa.isEmpty
      def bind[A, B](fa: XMap[K,A])(f: A => XMap[K, B]) = fa.collect{case (k, v) if f(v).isDefinedAt(k) => k -> f(v)(k)}
      override def map[A, B](fa: XMap[K, A])(f: A => B) = fa.transform{case (_, v) => f(v)}
      def traverseImpl[G[_],A,B](m: XMap[K,A])(f: A => G[B])(implicit G: Applicative[G]): G[XMap[K,B]] =
        G.map(list.listInstance.traverseImpl(m.toList)({ case (k, v) => G.map(f(v))(k -> _) }))(xs => fromSeq(xs:_*))
      import \&/._
      override def alignWith[A, B, C](f: A \&/ B => C) = {
        case (a, b) if b.isEmpty => map(a)(v => f(This(v)))
        case (a, b) if a.isEmpty => map(b)(v => f(That(v)))
        case (a, b) =>
          map(unionWith(map(a)(This(_): A \&/ B), map(b)(That(_): A \&/ B)){
            case (This(aa), That(bb)) => Both(aa, bb)
            case _ => sys.error("Map alignWith")
          })(f)
      }
      override def align[A, B](a: XMap[K, A], b: XMap[K, B]) = (a, b) match {
        case (a, b) if b.isEmpty => map(a)(This(_))
        case (a, b) if a.isEmpty => map(b)(That(_))
        case (a, b) =>
          unionWith(map(a)(This(_): A \&/ B), map(b)(That(_): A \&/ B)){
            case (This(aa), That(bb)) => Both(aa, bb)
            case _ => sys.error("Map align")
          }
      }
    }

  /** Map union monoid, unifying values with `V`'s `append`. */
  implicit def mapMonoid[K: BuildKeyConstraint, V: Semigroup]: Monoid[XMap[K, V]] =
    new Monoid[XMap[K, V]] {
      def zero = fromSeq[K, V]()
      def append(m1: XMap[K, V], m2: => XMap[K, V]) = {
        // Eagerly consume m2 as the value is used more than once.
        val m2Instance: XMap[K, V] = m2
        // semigroups are not commutative, so order may matter.
        val (from, to, semigroup) = {
          if (m1.size > m2Instance.size) (m2Instance, m1, Semigroup[V].append(_: V, _: V))
          else (m1, m2Instance, (Semigroup[V].append(_: V, _: V)).flip)
        }

        from.foldLeft(to) {
          case (to, (k, v)) => ab_+(to, k, to.get(k).map(semigroup(_, v)).getOrElse(v))
        }
      }
    }

  implicit def mapShow[K, V](implicit K: Show[K], V: Show[V]): Show[XMap[K, V]] =
    Show.show(m => "Map[" +:
                Cord.mkCord(", ", m.toSeq.view.map{
                  case (k, v) => Cord(K show k, "->", V show v)
                }: _*) :+ "]")

  implicit def mapOrder[K: Order, V: Order]: Order[XMap[K, V]] =
    new Order[XMap[K, V]] with MapEqual[K, V] {
      def OK = Order[K]
      def OV = Equal[V]
      def order(x: XMap[K, V], y: XMap[K, V]): Ordering = {
        import vector._
        import anyVal._
        import tuple._
        implicit val ok = Order[K].toScalaOrdering
        Semigroup[Ordering]
         .append(Order[Int].order(x.size, y.size),
                 Order[Vector[(K, V)]]
                  .order(x.toVector.sortBy((_:(K,V))._1),
                         y.toVector.sortBy((_:(K,V))._1)))
      }
    }
}

trait MapSubFunctions extends MapSub {
  /** Vary the value of `m get k`. */
  final def alter[K: BuildKeyConstraint, A](m: XMap[K, A], k: K)(f: (Option[A] => Option[A])): XMap[K, A] =
    f(m get k) map (ab_+(m, k, _)) getOrElse ab_-(m, k)

  /** Like `intersectWith`, but tell `f` about the key. */
  final def intersectWithKey[K:BuildKeyConstraint,A,B,C](m1: XMap[K, A], m2: XMap[K, B])(f: (K, A, B) => C): XMap[K, C] = m1 collect {
    case (k, v) if m2 contains k => k -> f(k, v, m2(k))
  }

  /** Collect only elements with matching keys, joining their
    * associated values with `f`.
    */
  final def intersectWith[K:BuildKeyConstraint,A,B,C](m1: XMap[K, A], m2: XMap[K, B])(f: (A, B) => C): XMap[K, C] =
    intersectWithKey(m1, m2)((_, x, y) => f(x, y))

  /** Exchange keys of `m` according to `f`.  Result may be smaller if
    * `f` maps two or more `K`s to the same `K2`, in which case the
    * resulting associated value is an arbitrary choice.
    */
  final def mapKeys[K, K2: BuildKeyConstraint, A](m: XMap[K, A])(f: K => K2): XMap[K2, A] =
    m map {case (k, v) => f(k) -> v}

  /** Like `unionWith`, but telling `f` about the key. */
  final def unionWithKey[K:BuildKeyConstraint,A](m1: XMap[K, A], m2: XMap[K, A])(f: (K, A, A) => A): XMap[K, A] = {
    val diff = m2 -- m1.keySet
    val aug = m1 transform {
      case (k, v) => if (m2 contains k) f(k, v, m2(k)) else v
    }
    aug ++ diff
  }

  /** Union, resolving collisions with `f`, where the first arg is
    * guaranteed to be from `m1`, the second from `m2`.
    *
    * @note iff `f` gives rise to a [[scalaz.Semigroup]], so does
    *       `unionWith(_, _)(f)`.*/
  final def unionWith[K:BuildKeyConstraint,A](m1: XMap[K, A], m2: XMap[K, A])(f: (A, A) => A): XMap[K, A] =
    unionWithKey(m1, m2)((_, x, y) => f(x, y))

  /** As with `Map.updated`, but resolve a collision with `f`.  The
    * first argument is guaranteed to be from `m1`.
    */
  final def insertWith[K:BuildKeyConstraint,A](m1: XMap[K, A], k: K, v: A)(f: (A, A) => A): XMap[K, A] =
    if(m1 contains k) ab_+(m1, k, f(m1(k), v)) else ab_+(m1, k, v)

  /** Grab a value out of Map if it's present. Otherwise evaluate
    * a value to be placed at that key in the Map.
    */
  final def getOrAdd[F[_],K,A](m: XMap[K, A], k: K)(fa: => F[A])(implicit F: Applicative[F], K: BuildKeyConstraint[K]): F[(XMap[K, A], A)] =
    (m get k).map(a => F.point(m, a)).getOrElse(F.map(fa)(a => (ab_+(m, k, a), a)))
}

trait MapInstances extends MapSubInstances with MapSubMap

trait MapFunctions extends MapSubFunctions with MapSubMap

object map extends MapInstances with MapFunctions
