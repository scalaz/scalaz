package scalaz

/** [[scala.Tuple2]], but with values by name. */
sealed trait LazyTuple2[A, B] {
  def _1: A

  def _2: B
}

/** [[scala.Tuple3]], but with values by name. */
sealed trait LazyTuple3[A, B, C] {
  def _1: A

  def _2: B

  def _3: C
}

/** [[scala.Tuple4]], but with values by name. */
sealed trait LazyTuple4[A, B, C, D] {
  def _1: A

  def _2: B

  def _3: C

  def _4: D
}

object LazyTuple extends LazyTupleFunctions

object LazyTuple2 extends LazyTuple2Instances {
  def apply[A, B](a: => A, b: => B): LazyTuple2[A, B] = new LazyTuple2[A, B] {
    def _1 = a
    def _2 = b
  }
}

object LazyTuple3 extends LazyTuple3Instances {
  def apply[A, B, C](a: => A, b: => B, c: => C): LazyTuple3[A, B, C] = new LazyTuple3[A, B, C] {
    def _1 = a
    def _2 = b
    def _3 = c
  }
}

object LazyTuple4 extends LazyTuple4Instances {
  def apply[A, B, C, D](a: => A, b: => B, c: => C, d: => D): LazyTuple4[A, B, C, D] = new LazyTuple4[A, B, C, D] {
    def _1 = a
    def _2 = b
    def _3 = c
    def _4 = d
  }
}

trait LazyTupleFunctions {
  type :&:[A, B] = LazyTuple2[A, B]

  def lazyTuple2[A, B](a: => A, b: => B): (A :&: B) = new (A :&: B) {
    def _1 = a

    def _2 = b
  }

  def lazyTuple3[A, B, C](a: => A, b: => B, c: => C): LazyTuple3[A, B, C] = new LazyTuple3[A, B, C] {
    def _1 = a

    def _2 = b

    def _3 = c
  }

  def lazyTuple4[A, B, C, D](a: => A, b: => B, c: => C, d: => D): LazyTuple4[A, B, C, D] = new LazyTuple4[A, B, C, D] {
    def _1 = a

    def _2 = b

    def _3 = c

    def _4 = d
  }
}

trait LazyTuple2Instances1 {
  implicit def lazyTuple2Instance[A1, A2] = new Bitraverse[LazyTuple2] {
    override def bimap[A, B, C, D](fab: LazyTuple2[A, B])(f: A => C, g: B => D): LazyTuple2[C, D] = LazyTuple.lazyTuple2(f(fab._1), g(fab._2))
    def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: LazyTuple2[A, B])(f: A => G[C], g: B => G[D]): G[LazyTuple2[C, D]] = {
      Applicative[G].apply2(f(fab._1), g(fab._2))(LazyTuple.lazyTuple2(_, _))
    }
  }

  implicit def lazyTuple2Semigroup[A1, A2](implicit A1: Semigroup[A1], A2: Semigroup[A2]) = new LazyTuple2Semigroup[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }

  implicit def lazyTuple2Functor[A1]: Functor[({type f[x] = LazyTuple2[A1, x]})#f] = new LazyTuple2Functor[A1] {}
}
trait LazyTuple2Instances0 extends LazyTuple2Instances1 {

  implicit def lazyTuple2Show[A1, A2](implicit A1: Show[A1], A2: Show[A2]) = new LazyTuple2Show[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }

  implicit def lazyTuple2Order[A1, A2](implicit A1: Order[A1], A2: Order[A2]) = new LazyTuple2Order[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }

  implicit def lazyTuple2Monoid[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]) = new LazyTuple2Monoid[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }

  implicit def lazyTuple2Monad[A1](implicit A1: Monoid[A1]): Monad[({type f[x] = LazyTuple2[A1, x]})#f] = new LazyTuple2Monad[A1] {
    implicit def _1 = A1
  }
}

trait LazyTuple2Instances extends LazyTuple2Instances0 {
  implicit def lazyTuple2Group[A1, A2](implicit A1: Group[A1], A2: Group[A2]) = new LazyTuple2Group[A1, A2] {
      implicit def _1 = A1
      implicit def _2 = A2
    }
}

trait LazyTuple3Instances1 {
  implicit def lazyTuple3Semigroup[A1, A2, A3](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3]) = new LazyTuple3Semigroup[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }

  implicit def lazyTuple3Functor[A1, A2]: Functor[({type f[x] = LazyTuple3[A1, A2, x]})#f] = new LazyTuple3Functor[A1, A2] {}

  implicit def lazyTuple3Equal[A1, A2, A3](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3]) = new LazyTuple3Equal[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }
}
trait LazyTuple3Instances0 extends LazyTuple3Instances1 {

  implicit def lazyTuple3Show[A1, A2, A3](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3]) = new LazyTuple3Show[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }

  implicit def lazyTuple3Order[A1, A2, A3](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3]) = new LazyTuple3Order[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }

  implicit def lazyTuple3Monoid[A1, A2, A3](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3]) = new LazyTuple3Monoid[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }

  implicit def lazyTuple3Monad[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]): Monad[({type f[x] = LazyTuple3[A1, A2, x]})#f] = new LazyTuple3Monad[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }
}

trait LazyTuple3Instances extends LazyTuple3Instances0 {
  implicit def lazyTuple3Group[A1, A2, A3](implicit A1: Group[A1], A2: Group[A2], A3: Group[A3]) = new LazyTuple3Group[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }
}

trait LazyTuple4Instances1 {
  implicit def lazyTuple4Semigroup[A1, A2, A3, A4](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4]) = new LazyTuple4Semigroup[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }

  implicit def lazyTuple4Functor[A1, A2, A3]: Functor[({type f[x] = LazyTuple4[A1, A2, A3, x]})#f] = new LazyTuple4Functor[A1, A2, A3] {}

  implicit def lazyTuple4Equal[A1, A2, A3, A4](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4]) = new LazyTuple4Equal[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }
}
trait LazyTuple4Instances0 extends LazyTuple4Instances1 {

  implicit def lazyTuple4Show[A1, A2, A3, A4](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4]) = new LazyTuple4Show[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }

  implicit def lazyTuple4Order[A1, A2, A3, A4](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4]) = new LazyTuple4Order[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }

  implicit def lazyTuple4Monoid[A1, A2, A3, A4](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4]) = new LazyTuple4Monoid[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }

  implicit def lazyTuple4Monad[A1, A2, A3](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3]): Monad[({type f[x] = LazyTuple4[A1, A2, A3, x]})#f] = new LazyTuple4Monad[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }
}

trait LazyTuple4Instances extends LazyTuple4Instances0 {
  implicit def lazyTuple4Group[A1, A2, A3, A4](implicit A1: Group[A1], A2: Group[A2], A3: Group[A3], A4: Group[A4]) = new LazyTuple4Group[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }
}

////

private[scalaz] trait LazyTuple2Functor[A1] extends Functor[({type f[x] = LazyTuple2[A1, x]})#f] {
  override def map[A, B](fa: LazyTuple2[A1, A])(f: A => B) =
    LazyTuple2(fa._1, f(fa._2))
}
private[scalaz] trait LazyTuple3Functor[A1, A2] extends Functor[({type f[x] = LazyTuple3[A1, A2, x]})#f] {
  override def map[A, B](fa: LazyTuple3[A1, A2, A])(f: A => B) =
    LazyTuple3(fa._1, fa._2, f(fa._3))
}
private[scalaz] trait LazyTuple4Functor[A1, A2, A3] extends Functor[({type f[x] = LazyTuple4[A1, A2, A3, x]})#f] {
  override def map[A, B](fa: LazyTuple4[A1, A2, A3, A])(f: A => B) =
    LazyTuple4(fa._1, fa._2, fa._3, f(fa._4))
}

import LazyTuple._

private[scalaz] trait LazyTuple2Semigroup[A1, A2] extends Semigroup[LazyTuple2[A1, A2]] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  def append(f1: LazyTuple2[A1, A2], f2: => LazyTuple2[A1, A2]) = LazyTuple2(
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2)
    )
}
private[scalaz] trait LazyTuple3Semigroup[A1, A2, A3] extends Semigroup[LazyTuple3[A1, A2, A3]] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  def append(f1: LazyTuple3[A1, A2, A3], f2: => LazyTuple3[A1, A2, A3]) = LazyTuple3(
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2),
    _3.append(f1._3, f2._3)
    )
}
private[scalaz] trait LazyTuple4Semigroup[A1, A2, A3, A4] extends Semigroup[LazyTuple4[A1, A2, A3, A4]] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  def append(f1: LazyTuple4[A1, A2, A3, A4], f2: => LazyTuple4[A1, A2, A3, A4]) = LazyTuple4(
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2),
    _3.append(f1._3, f2._3),
    _4.append(f1._4, f2._4)
    )
}


private[scalaz] trait LazyTuple2Equal[A1, A2] extends Equal[LazyTuple2[A1, A2]] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  override def equal(f1: LazyTuple2[A1, A2], f2: LazyTuple2[A1, A2]) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2)
}
private[scalaz] trait LazyTuple3Equal[A1, A2, A3] extends Equal[LazyTuple3[A1, A2, A3]] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  override def equal(f1: LazyTuple3[A1, A2, A3], f2: LazyTuple3[A1, A2, A3]) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3)
}
private[scalaz] trait LazyTuple4Equal[A1, A2, A3, A4] extends Equal[LazyTuple4[A1, A2, A3, A4]] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  override def equal(f1: LazyTuple4[A1, A2, A3, A4], f2: LazyTuple4[A1, A2, A3, A4]) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4)
}

private[scalaz] trait LazyTuple2Show[A1, A2] extends Show[LazyTuple2[A1, A2]] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  override def show(f: LazyTuple2[A1, A2]) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ")")
}
private[scalaz] trait LazyTuple3Show[A1, A2, A3] extends Show[LazyTuple3[A1, A2, A3]] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  override def show(f: LazyTuple3[A1, A2, A3]) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ")")
}
private[scalaz] trait LazyTuple4Show[A1, A2, A3, A4] extends Show[LazyTuple4[A1, A2, A3, A4]] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  override def show(f: LazyTuple4[A1, A2, A3, A4]) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ",", _4.show(f._4), ")")
}

private[scalaz] trait LazyTuple2Order[A1, A2] extends Order[LazyTuple2[A1, A2]] with LazyTuple2Equal[A1, A2] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  import Ordering.EQ
  def order(f1: LazyTuple2[A1, A2], f2: LazyTuple2[A1, A2]) =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2)) match {
      case (EQ, ord) => ord
      case (ord, _) => ord
    }
}
private[scalaz] trait LazyTuple3Order[A1, A2, A3] extends Order[LazyTuple3[A1, A2, A3]] with LazyTuple3Equal[A1, A2, A3]{
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  import Ordering.EQ
  def order(f1: LazyTuple3[A1, A2, A3], f2: LazyTuple3[A1, A2, A3]) =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3)) match {
      case (EQ, EQ, ord) => ord
      case (EQ, ord, _) => ord
      case (ord, _, _) => ord
    }
}
private[scalaz] trait LazyTuple4Order[A1, A2, A3, A4] extends Order[LazyTuple4[A1, A2, A3, A4]] with LazyTuple4Equal[A1, A2, A3, A4]{
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  import Ordering.EQ
  def order(f1: LazyTuple4[A1, A2, A3, A4], f2: LazyTuple4[A1, A2, A3, A4]) =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3), _4.order(f1._4, f2._4)) match {
      case (EQ, EQ, EQ, ord) => ord
      case (EQ, EQ, ord, _) => ord
      case (EQ, ord, _, _) => ord
      case (ord, _, _, _) => ord
    }
}

private[scalaz] trait LazyTuple2Monoid[A1, A2] extends Monoid[LazyTuple2[A1, A2]] with LazyTuple2Semigroup[A1, A2] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  def zero: LazyTuple2[A1, A2] = LazyTuple2(_1.zero, _2.zero)
}
private[scalaz] trait LazyTuple3Monoid[A1, A2, A3] extends Monoid[LazyTuple3[A1, A2, A3]] with LazyTuple3Semigroup[A1, A2, A3] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  def zero: LazyTuple3[A1, A2, A3] = LazyTuple3(_1.zero, _2.zero, _3.zero)
}
private[scalaz] trait LazyTuple4Monoid[A1, A2, A3, A4] extends Monoid[LazyTuple4[A1, A2, A3, A4]] with LazyTuple4Semigroup[A1, A2, A3, A4] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  def zero: LazyTuple4[A1, A2, A3, A4] = LazyTuple4(_1.zero, _2.zero, _3.zero, _4.zero)
}

private[scalaz] trait LazyTuple2Group[A1, A2] extends Group[LazyTuple2[A1, A2]] with LazyTuple2Monoid[A1, A2] {
  implicit def _1 : Group[A1]
  implicit def _2 : Group[A2]
  def inverse(f: LazyTuple2[A1, A2]) = LazyTuple2(_1.inverse(f._1), _2.inverse(f._2))
}
private[scalaz] trait LazyTuple3Group[A1, A2, A3] extends Group[LazyTuple3[A1, A2, A3]] with LazyTuple3Monoid[A1, A2, A3] {
  implicit def _1 : Group[A1]
  implicit def _2 : Group[A2]
  implicit def _3 : Group[A3]
  def inverse(f: LazyTuple3[A1, A2, A3]) = LazyTuple3(_1.inverse(f._1), _2.inverse(f._2), _3.inverse(f._3))
}
private[scalaz] trait LazyTuple4Group[A1, A2, A3, A4] extends Group[LazyTuple4[A1, A2, A3, A4]] with LazyTuple4Monoid[A1, A2, A3, A4] {
  implicit def _1 : Group[A1]
  implicit def _2 : Group[A2]
  implicit def _3 : Group[A3]
  implicit def _4 : Group[A4]
  def inverse(f: LazyTuple4[A1, A2, A3, A4]) = lazyTuple4(_1.inverse(f._1), _2.inverse(f._2), _3.inverse(f._3), _4.inverse(f._4))
}


// LazyTupleN forms a Monad if the element types other than the last are Monoids.

private[scalaz] trait LazyTuple2Monad[A1] extends Monad[({type f[x] = LazyTuple2[A1, x]})#f] with LazyTuple2Functor[A1] {
  implicit def _1 : Monoid[A1]
  def bind[A, B](fa: LazyTuple2[A1, A])(f: A => LazyTuple2[A1, B]) = {
    val t = f(fa._2)

    lazyTuple2(_1.append(fa._1, t._1), t._2)
  }
  def point[A](a: => A) = lazyTuple2(_1.zero, a)
}
private[scalaz] trait LazyTuple3Monad[A1, A2] extends Monad[({type f[x] = LazyTuple3[A1, A2, x]})#f] with LazyTuple3Functor[A1, A2] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  def bind[A, B](fa: LazyTuple3[A1, A2, A])(f: A => LazyTuple3[A1, A2, B]) = {
    val t = f(fa._3)

    lazyTuple3(_1.append(fa._1, t._1), _2.append(fa._2, t._2), t._3)
  }

  def point[A](a: => A) = lazyTuple3(_1.zero, _2.zero, a)
}
private[scalaz] trait LazyTuple4Monad[A1, A2, A3] extends Monad[({type f[x] = LazyTuple4[A1, A2, A3, x]})#f] with LazyTuple4Functor[A1, A2, A3] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  def bind[A, B](fa: LazyTuple4[A1, A2, A3, A])(f: A => LazyTuple4[A1, A2, A3, B]) = {
    val t = f(fa._4)

    lazyTuple4(_1.append(fa._1, t._1), _2.append(fa._2, t._2), _3.append(fa._3, t._3), t._4)
  }
  def point[A](a: => A) = lazyTuple4(_1.zero, _2.zero, _3.zero, a)
}
