package scalaz
package std

trait TupleInstances0 {
  implicit def tuple1Semigroup[A1: Semigroup] = new Semigroup[Tuple1[A1]] {
    def append(f1: Tuple1[A1], f2: => Tuple1[A1]): Tuple1[A1] = (
      Tuple1(Semigroup[A1].append(f1._1, f2._1))
    )
  }
  
  implicit def tuple2Semigroup[A1, A2](implicit A1: Semigroup[A1], A2: Semigroup[A2]) = new Tuple2Semigroup[A1, A2] {
    implicit def _1: Semigroup[A1] = A1
    implicit def _2: Semigroup[A2] = A2
  }
  
  implicit def tuple2Instance[A1, A2] = new BiTraverse[Tuple2] {
    override def bimap[A, B, C, D](fab: (A, B))(f: (A) => C, g: (B) => D): (C, D) = (f(fab._1), g(fab._2))
    def bitraverse[G[_]: Applicative, A, B, C, D](fab: (A, B))(f: (A) => G[C], g: (B) => G[D]): G[(C, D)] = {
      Applicative[G].lift2((c: C, d: D) => (c, d))(f(fab._1), g(fab._2))
    }
  }

  implicit def tupleFunctor[S]: Functor[({type f[x] = (S, x)})#f] = new Functor[({type f[x] = (S, x)})#f] {
    def map[A, B](t: (S, A))(f: A => B): (S, B) =
      (t._1, f(t._2))
  }
}

trait TupleInstances extends TupleInstances0 {
  implicit def tuple2Monoid[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]) = new Tuple2Monoid[A1, A2] {
    implicit def _1: Monoid[A1] = A1
    implicit def _2: Monoid[A2] = A2
  }
  implicit def tuple2Order[A1, A2](implicit A1: Order[A1], A2: Order[A2]) = new Tuple2Order[A1, A2] {
    implicit def _1: Order[A1] = A1
    implicit def _2: Order[A2] = A2
  }
  implicit def tuple2Show[A1, A2](implicit A1: Show[A1], A2: Show[A2]) = new Tuple2Show[A1, A2] {
    implicit def _1: Show[A1] = A1
    implicit def _2: Show[A2] = A2
  }
  implicit def tuple3Monoid[A1, A2, A3](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3]) = new Tuple3Monoid[A1, A2, A3] {
    implicit def _1: Monoid[A1] = A1
    implicit def _2: Monoid[A2] = A2
    implicit def _3: Monoid[A3] = A3
  }
  implicit def tuple3Order[A1, A2, A3](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3]) = new Tuple3Order[A1, A2, A3] {
    implicit def _1: Order[A1] = A1
    implicit def _2: Order[A2] = A2
    implicit def _3: Order[A3] = A3
  }
  implicit def tuple3Show[A1, A2, A3](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3]) = new Tuple3Show[A1, A2, A3] {
    implicit def _1: Show[A1] = A1
    implicit def _2: Show[A2] = A2
    implicit def _3: Show[A3] = A3
  }
  // TODO Show, Equal/Order
  // TODO pump up the arity.
}

object tuple extends TupleInstances

//
// Type class implementation traits
//

private[scalaz] trait Tuple2Semigroup[A1, A2] extends Semigroup[(A1, A2)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  
  def append(f1: (A1, A2), f2: => (A1, A2)): (A1, A2) = (
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2)
  )
}

private[scalaz] trait Tuple2Monoid[A1, A2] extends Tuple2Semigroup[A1, A2] with Monoid[(A1, A2)] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]

  def zero: (A1, A2) = (_1.zero, _2.zero)
}

private[scalaz] trait Tuple2Order[A1, A2] extends Order[(A1, A2)] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]

  def order(f1: (A1, A2), f2: (A1, A2)) = (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2)) match {
    case (Ordering.EQ, ord) => ord
    case (ord, _) => ord
  }
}

private[scalaz] trait Tuple2Show[A1, A2] extends Show[(A1, A2)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]

  def show(f: (A1, A2)) = "(".toList ::: _1.show(f._1) ::: ",".toList ::: _2.show(f._2) ::: ")".toList
}

private[scalaz] trait Tuple3Semigroup[A1, A2, A3] extends Semigroup[(A1, A2, A3)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]

  def append(f1: (A1, A2, A3), f2: => (A1, A2, A3)): (A1, A2, A3) = (
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2),
    _3.append(f1._3, f2._3)
  )
}
private[scalaz] trait Tuple3Monoid[A1, A2, A3] extends Tuple3Semigroup[A1, A2, A3] with Monoid[(A1, A2, A3)] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]

  def zero: (A1, A2, A3) = (_1.zero, _2.zero, _3.zero)
}

private[scalaz] trait Tuple3Order[A1, A2, A3] extends Order[(A1, A2, A3)] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]

  import Ordering._

  def order(f1: (A1, A2, A3), f2: (A1, A2, A3)) = (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3)) match {
    case (EQ, EQ, ord) => ord
    case (EQ, ord, _) => ord
    case (ord, _, _) => ord
  }
}
private[scalaz] trait Tuple3Show[A1, A2, A3] extends Show[(A1, A2, A3)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]

  def show(f: (A1, A2, A3)) = "(".toList ::: _1.show(f._1) ::: ",".toList ::: _2.show(f._2) ::: ",".toList ::: _3.show(f._3) ::: ")".toList
}


/* implicit def tuple7Semigroup[A1: Semigroup, A2: Semigroup, A3: Semigroup, A4: Semigroup, A5: Semigroup, A6: Semigroup, A7: Semigroup] = new Semigroup[(A1, A2, A3, A4, A5, A6, A7)] {
  def append(f1: (A1, A2, A3, A4, A5, A6, A7), f2: => (A1, A2, A3, A4, A5, A6, A7)): (A1, A2, A3, A4, A5, A6, A7) = (
    Semigroup[A1].append(f1._1, f2._1),
    Semigroup[A2].append(f1._2, f2._2),
    Semigroup[A3].append(f1._3, f2._3),
    Semigroup[A4].append(f1._4, f2._4),
    Semigroup[A5].append(f1._5, f2._5),
    Semigroup[A6].append(f1._6, f2._6),
    Semigroup[A7].append(f1._7, f2._7)
  )
}*/
