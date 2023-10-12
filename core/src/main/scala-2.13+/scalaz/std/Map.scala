package scalaz
package std

trait MapInstances0 {
  import syntax.std.function2._
  private[std] sealed trait MapMonoid[K, V] extends Monoid[Map[K, V]] {
    implicit def V: Semigroup[V]

    def zero = Map.empty[K, V]
    def append(m1: Map[K, V], m2: => Map[K, V]) = {
      // Eagerly consume m2 as the value is used more than once.
      val m2Instance: Map[K, V] = m2
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

  private[std] trait MapEqual[K, V] extends Equal[Map[K, V]] {
    implicit def OK: Order[K]
    implicit def OV: Equal[V]

    override def equal(a1: Map[K, V], a2: Map[K, V]): Boolean = {
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

  private[std] trait MapFoldable[K] extends Foldable.FromFoldr[Map[K, *]] {
    override def foldLeft[A, B](fa: Map[K, A], z: B)(f: (B, A) => B) =
      fa.valuesIterator.foldLeft(z)(f)

    override def foldRight[A, B](fa: Map[K, A], z: => B)(f: (A, => B) => B) =
      fa.foldRight(z)((p, b) => f(p._2, b))

    override final def all[A](fa: Map[K, A])(f: A => Boolean) =
      fa.valuesIterator.forall(f)

    override final def any[A](fa: Map[K, A])(f: A => Boolean) =
      fa.valuesIterator.exists(f)
  }

  implicit def mapEqual[K: Order, V: Equal]: Equal[Map[K, V]] =
    new MapEqual[K, V] {
      def OK = Order[K]
      def OV = Equal[V]
    }

  implicit def mapFoldable[K]: Foldable[Map[K, *]] =
    new MapFoldable[K]{}

  implicit def mapBand[K, V](implicit S: Band[V]): Band[Map[K, V]] =
    new MapMonoid[K, V] with Band[Map[K, V]] {
      implicit override def V: Band[V] = S
    }
}

trait MapInstances extends MapInstances0 with MapFunctions {
  import Liskov.<~<

  /** Covariant over the value parameter, where `plus` applies the
    * `Last` semigroup to values.
    */
  implicit def mapInstance[K]: Traverse[Map[K, *]] with IsEmpty[Map[K, *]] with Bind[Map[K, *]] with Align[Map[K, *]] =
    new Traverse[Map[K, *]] with IsEmpty[Map[K, *]] with Bind[Map[K, *]] with MapFoldable[K] with Align[Map[K, *]] {
      def empty[V] = Map.empty[K, V]
      def plus[V](a: Map[K, V], b: => Map[K, V]) = a ++ b
      def isEmpty[V](fa: Map[K, V]) = fa.isEmpty
      def bind[A, B](fa: Map[K,A])(f: A => Map[K, B]) = fa.collect{case (k, v) if f(v).isDefinedAt(k) => k -> f(v)(k)}
      override def map[A, B](fa: Map[K, A])(f: A => B) = fa.transform{case (_, v) => f(v)}
      override def widen[A, B](fa: Map[K, A])(implicit ev: A <~< B) = Liskov.co[Map[K, +*], A, B](ev)(fa)
      def traverseImpl[G[_],A,B](m: Map[K,A])(f: A => G[B])(implicit G: Applicative[G]): G[Map[K,B]] =
        G.map(list.listInstance.traverseImpl(m.toList)({ case (k, v) => G.map(f(v))(k -> _) }))(xs => Map(xs:_*))
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
      override def align[A, B](a: Map[K, A], b: Map[K, B]) = (a, b) match {
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
  implicit def mapMonoid[K, V](implicit S: Semigroup[V]): Monoid[Map[K, V]] =
    new MapMonoid[K, V] { self =>
      override def V = S
    }

  implicit def mapShow[K, V](implicit K: Show[K], V: Show[V]): Show[Map[K, V]] =
    Show.show(m => "Map[" +:
                Cord.mkCord(", ", m.toSeq.map{
                  case (k, v) => Cord(K show k, "->", V show v)
                }: _*) :+ "]")

  implicit def mapOrder[K: Order, V: Order]: Order[Map[K, V]] =
    new Order[Map[K, V]] with MapEqual[K, V] {
      override def OK: Order[K] = Order[K]
      override def OV: Equal[V] = Equal[V]
      def order(x: Map[K, V], y: Map[K, V]): Ordering = {
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

trait MapFunctions {
  /** Vary the value of `m get k`. */
  final def alter[K, A](m: Map[K, A], k: K)(f: (Option[A] => Option[A])): Map[K, A] =
    f(m get k) map (x => m + ((k, x))) getOrElse (m - k)

  /** Like `intersectWith`, but tell `f` about the key. */
  final def intersectWithKey[K, A, B, C](m1: Map[K, A], m2: Map[K, B])(f: (K, A, B) => C): Map[K, C] = m1 collect {
    case (k, v) if m2 contains k => k -> f(k, v, m2(k))
  }

  /** Collect only elements with matching keys, joining their
    * associated values with `f`.
    */
  final def intersectWith[K, A, B, C](m1: Map[K, A], m2: Map[K, B])(f: (A, B) => C): Map[K, C] =
    intersectWithKey(m1, m2)((_, x, y) => f(x, y))

  /** Exchange keys of `m` according to `f`.  Result may be smaller if
    * `f` maps two or more `K`s to the same `K2`, in which case the
    * resulting associated value is an arbitrary choice.
    */
  final def mapKeys[K, K2, A](m: Map[K, A])(f: K => K2): Map[K2, A] =
    m map {case (k, v) => f(k) -> v}

  /** Like `unionWith`, but telling `f` about the key. */
  final def unionWithKey[K, A](m1: Map[K, A], m2: Map[K, A])(f: (K, A, A) => A): Map[K, A] = {
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
  final def unionWith[K, A](m1: Map[K, A], m2: Map[K, A])(f: (A, A) => A): Map[K, A] =
    unionWithKey(m1, m2)((_, x, y) => f(x, y))

  /** As with `Map.updated`, but resolve a collision with `f`.  The
    * first argument is guaranteed to be from `m1`.
    */
  final def insertWith[K, A](m1: Map[K, A], k: K, v: A)(f: (A, A) => A): Map[K, A] =
    if(m1 contains k) m1 + ((k, f(m1(k), v))) else m1 + ((k, v))

  /** Grab a value out of Map if it's present. Otherwise evaluate
    * a value to be placed at that key in the Map.
    */
  final def getOrAdd[F[_], K, A](m: Map[K, A], k: K)(fa: => F[A])(implicit F: Applicative[F]): F[(Map[K, A], A)] =
    (m get k).fold(F.map(fa)(a => (m + ((k, a)), a)))(a => F.point((m, a)))
}

object map extends MapInstances with MapFunctions
