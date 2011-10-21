package scalaz
package std

trait TuplesLow0 {
  implicit def tuple1Semigroup[A1: Semigroup] = new Semigroup[Tuple1[A1]] {
    def append(f1: Tuple1[A1], f2: => Tuple1[A1]): Tuple1[A1] = (
      Tuple1(Semigroup[A1].append(f1._1, f2._1))
    )
  }
  
  implicit def tuple2Semigroup[A1, A2](implicit A1: Semigroup[A1], A2: Semigroup[A2]) = new Tuple2Semigroup[A1, A2] {
    implicit def _1: Semigroup[A1] = A1
    implicit def _2: Semigroup[A2] = A2
  }
  
  implicit def tuple2[A1, A2] = new BiFunctor[Tuple2] {
    def bimap[A, B, C, D](fab: (A, B))(f: (A) => C, g: (B) => D): (C, D) = (f(fab._1), g(fab._2))
  }
}

trait Tuples extends TuplesLow0 {
  implicit def tuple2Monoid[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]) = new Tuple2Monoid[A1, A2] {
    implicit def _1: Monoid[A1] = A1
    implicit def _2: Monoid[A2] = A2
  }
  // TODO Show, Equal/Order
  // TODO pump up the arity.
}

object Tuple extends Tuples {
  
}

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