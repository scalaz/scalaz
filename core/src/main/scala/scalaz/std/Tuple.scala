package scalaz
package std

trait TupleInstances0 {
  implicit def tuple2Instance[A1, A2] = new BiTraverse[Tuple2] {
    override def bimap[A, B, C, D](fab: (A, B))(f: (A) => C, g: (B) => D): (C, D) = (f(fab._1), g(fab._2))
    def bitraverse[G[_]: Applicative, A, B, C, D](fab: (A, B))(f: (A) => G[C], g: (B) => G[D]): G[(C, D)] = {
      Applicative[G].lift2((c: C, d: D) => (c, d))(f(fab._1), g(fab._2))
    }
  }

  implicit def tuple1Semigroup[A1](implicit A1: Semigroup[A1]) = new Tuple1Semigroup[A1] {
    implicit def _1: Semigroup[A1] = A1
  }
  implicit def tuple2Semigroup[A1, A2](implicit A1: Semigroup[A1], A2: Semigroup[A2]) = new Tuple2Semigroup[A1, A2] {
    implicit def _1 : Semigroup[A1] = A1
    implicit def _2 : Semigroup[A2] = A2
  }
  implicit def tuple3Semigroup[A1, A2, A3](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3]) = new Tuple3Semigroup[A1, A2, A3] {
    implicit def _1 : Semigroup[A1] = A1
    implicit def _2 : Semigroup[A2] = A2
    implicit def _3 : Semigroup[A3] = A3
  }
  implicit def tuple4Semigroup[A1, A2, A3, A4](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4]) = new Tuple4Semigroup[A1, A2, A3, A4] {
    implicit def _1 : Semigroup[A1] = A1
    implicit def _2 : Semigroup[A2] = A2
    implicit def _3 : Semigroup[A3] = A3
    implicit def _4 : Semigroup[A4] = A4
  }
  implicit def tuple5Semigroup[A1, A2, A3, A4, A5](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5]) = new Tuple5Semigroup[A1, A2, A3, A4, A5] {
    implicit def _1 : Semigroup[A1] = A1
    implicit def _2 : Semigroup[A2] = A2
    implicit def _3 : Semigroup[A3] = A3
    implicit def _4 : Semigroup[A4] = A4
    implicit def _5 : Semigroup[A5] = A5
  }
  implicit def tuple6Semigroup[A1, A2, A3, A4, A5, A6](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5], A6: Semigroup[A6]) = new Tuple6Semigroup[A1, A2, A3, A4, A5, A6] {
    implicit def _1 : Semigroup[A1] = A1
    implicit def _2 : Semigroup[A2] = A2
    implicit def _3 : Semigroup[A3] = A3
    implicit def _4 : Semigroup[A4] = A4
    implicit def _5 : Semigroup[A5] = A5
    implicit def _6 : Semigroup[A6] = A6
  }
  implicit def tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5], A6: Semigroup[A6], A7: Semigroup[A7]) = new Tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 : Semigroup[A1] = A1
    implicit def _2 : Semigroup[A2] = A2
    implicit def _3 : Semigroup[A3] = A3
    implicit def _4 : Semigroup[A4] = A4
    implicit def _5 : Semigroup[A5] = A5
    implicit def _6 : Semigroup[A6] = A6
    implicit def _7 : Semigroup[A7] = A7
  }
  implicit def tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5], A6: Semigroup[A6], A7: Semigroup[A7], A8: Semigroup[A8]) = new Tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8] {
    implicit def _1 : Semigroup[A1] = A1
    implicit def _2 : Semigroup[A2] = A2
    implicit def _3 : Semigroup[A3] = A3
    implicit def _4 : Semigroup[A4] = A4
    implicit def _5 : Semigroup[A5] = A5
    implicit def _6 : Semigroup[A6] = A6
    implicit def _7 : Semigroup[A7] = A7
    implicit def _8 : Semigroup[A8] = A8
  }
  implicit def tuple1Functor: Functor[Tuple1] = new Tuple1Functor {}
  implicit def tuple2Functor[A1]: Functor[({type f[x] = (A1, x)})#f] = new Tuple2Functor[A1] {}
  implicit def tuple3Functor[A1, A2]: Functor[({type f[x] = (A1, A2, x)})#f] = new Tuple3Functor[A1, A2] {}
  implicit def tuple4Functor[A1, A2, A3]: Functor[({type f[x] = (A1, A2, A3, x)})#f] = new Tuple4Functor[A1, A2, A3] {}
  implicit def tuple5Functor[A1, A2, A3, A4]: Functor[({type f[x] = (A1, A2, A3, A4, x)})#f] = new Tuple5Functor[A1, A2, A3, A4] {}
  implicit def tuple6Functor[A1, A2, A3, A4, A5]: Functor[({type f[x] = (A1, A2, A3, A4, A5, x)})#f] = new Tuple6Functor[A1, A2, A3, A4, A5] {}
  implicit def tuple7Functor[A1, A2, A3, A4, A5, A6]: Functor[({type f[x] = (A1, A2, A3, A4, A5, A6, x)})#f] = new Tuple7Functor[A1, A2, A3, A4, A5, A6] {}
  implicit def tuple8Functor[A1, A2, A3, A4, A5, A6, A7]: Functor[({type f[x] = (A1, A2, A3, A4, A5, A6, A7, x)})#f] = new Tuple8Functor[A1, A2, A3, A4, A5, A6, A7] {}

  implicit def tuple1Equal[A1](implicit A1: Equal[A1]) = new Tuple1Equal[A1] {
    implicit def _1 : Equal[A1] = A1
  }
  implicit def tuple2Equal[A1, A2](implicit A1: Equal[A1], A2: Equal[A2]) = new Tuple2Equal[A1, A2] {
    implicit def _1 : Equal[A1] = A1
    implicit def _2 : Equal[A2] = A2
  }
  implicit def tuple3Equal[A1, A2, A3](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3]) = new Tuple3Equal[A1, A2, A3] {
    implicit def _1 : Equal[A1] = A1
    implicit def _2 : Equal[A2] = A2
    implicit def _3 : Equal[A3] = A3
  }
  implicit def tuple4Equal[A1, A2, A3, A4](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4]) = new Tuple4Equal[A1, A2, A3, A4] {
    implicit def _1 : Equal[A1] = A1
    implicit def _2 : Equal[A2] = A2
    implicit def _3 : Equal[A3] = A3
    implicit def _4 : Equal[A4] = A4
  }
  implicit def tuple5Equal[A1, A2, A3, A4, A5](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5]) = new Tuple5Equal[A1, A2, A3, A4, A5] {
    implicit def _1 : Equal[A1] = A1
    implicit def _2 : Equal[A2] = A2
    implicit def _3 : Equal[A3] = A3
    implicit def _4 : Equal[A4] = A4
    implicit def _5 : Equal[A5] = A5
  }
  implicit def tuple6Equal[A1, A2, A3, A4, A5, A6](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5], A6: Equal[A6]) = new Tuple6Equal[A1, A2, A3, A4, A5, A6] {
    implicit def _1 : Equal[A1] = A1
    implicit def _2 : Equal[A2] = A2
    implicit def _3 : Equal[A3] = A3
    implicit def _4 : Equal[A4] = A4
    implicit def _5 : Equal[A5] = A5
    implicit def _6 : Equal[A6] = A6
  }
  implicit def tuple7Equal[A1, A2, A3, A4, A5, A6, A7](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5], A6: Equal[A6], A7: Equal[A7]) = new Tuple7Equal[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 : Equal[A1] = A1
    implicit def _2 : Equal[A2] = A2
    implicit def _3 : Equal[A3] = A3
    implicit def _4 : Equal[A4] = A4
    implicit def _5 : Equal[A5] = A5
    implicit def _6 : Equal[A6] = A6
    implicit def _7 : Equal[A7] = A7
  }
  implicit def tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5], A6: Equal[A6], A7: Equal[A7], A8: Equal[A8]) = new Tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8] {
    implicit def _1 : Equal[A1] = A1
    implicit def _2 : Equal[A2] = A2
    implicit def _3 : Equal[A3] = A3
    implicit def _4 : Equal[A4] = A4
    implicit def _5 : Equal[A5] = A5
    implicit def _6 : Equal[A6] = A6
    implicit def _7 : Equal[A7] = A7
    implicit def _8 : Equal[A8] = A8
  }
}
trait TupleInstances1 extends TupleInstances0 {

  implicit def tuple1Show[A1](implicit A1: Show[A1]) = new Tuple1Show[A1] {
    implicit def _1 : Show[A1] = A1
  }
  implicit def tuple2Show[A1, A2](implicit A1: Show[A1], A2: Show[A2]) = new Tuple2Show[A1, A2] {
    implicit def _1 : Show[A1] = A1
    implicit def _2 : Show[A2] = A2
  }
  implicit def tuple3Show[A1, A2, A3](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3]) = new Tuple3Show[A1, A2, A3] {
    implicit def _1 : Show[A1] = A1
    implicit def _2 : Show[A2] = A2
    implicit def _3 : Show[A3] = A3
  }
  implicit def tuple4Show[A1, A2, A3, A4](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4]) = new Tuple4Show[A1, A2, A3, A4] {
    implicit def _1 : Show[A1] = A1
    implicit def _2 : Show[A2] = A2
    implicit def _3 : Show[A3] = A3
    implicit def _4 : Show[A4] = A4
  }
  implicit def tuple5Show[A1, A2, A3, A4, A5](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5]) = new Tuple5Show[A1, A2, A3, A4, A5] {
    implicit def _1 : Show[A1] = A1
    implicit def _2 : Show[A2] = A2
    implicit def _3 : Show[A3] = A3
    implicit def _4 : Show[A4] = A4
    implicit def _5 : Show[A5] = A5
  }
  implicit def tuple6Show[A1, A2, A3, A4, A5, A6](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5], A6: Show[A6]) = new Tuple6Show[A1, A2, A3, A4, A5, A6] {
    implicit def _1 : Show[A1] = A1
    implicit def _2 : Show[A2] = A2
    implicit def _3 : Show[A3] = A3
    implicit def _4 : Show[A4] = A4
    implicit def _5 : Show[A5] = A5
    implicit def _6 : Show[A6] = A6
  }
  implicit def tuple7Show[A1, A2, A3, A4, A5, A6, A7](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5], A6: Show[A6], A7: Show[A7]) = new Tuple7Show[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 : Show[A1] = A1
    implicit def _2 : Show[A2] = A2
    implicit def _3 : Show[A3] = A3
    implicit def _4 : Show[A4] = A4
    implicit def _5 : Show[A5] = A5
    implicit def _6 : Show[A6] = A6
    implicit def _7 : Show[A7] = A7
  }
  implicit def tuple8Show[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5], A6: Show[A6], A7: Show[A7], A8: Show[A8]) = new Tuple8Show[A1, A2, A3, A4, A5, A6, A7, A8] {
    implicit def _1 : Show[A1] = A1
    implicit def _2 : Show[A2] = A2
    implicit def _3 : Show[A3] = A3
    implicit def _4 : Show[A4] = A4
    implicit def _5 : Show[A5] = A5
    implicit def _6 : Show[A6] = A6
    implicit def _7 : Show[A7] = A7
    implicit def _8 : Show[A8] = A8
  }

  implicit def tuple1Order[A1](implicit A1: Order[A1]) = new Tuple1Order[A1] {
    implicit def _1 : Order[A1] = A1
  }
  implicit def tuple2Order[A1, A2](implicit A1: Order[A1], A2: Order[A2]) = new Tuple2Order[A1, A2] {
    implicit def _1 : Order[A1] = A1
    implicit def _2 : Order[A2] = A2
  }
  implicit def tuple3Order[A1, A2, A3](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3]) = new Tuple3Order[A1, A2, A3] {
    implicit def _1 : Order[A1] = A1
    implicit def _2 : Order[A2] = A2
    implicit def _3 : Order[A3] = A3
  }
  implicit def tuple4Order[A1, A2, A3, A4](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4]) = new Tuple4Order[A1, A2, A3, A4] {
    implicit def _1 : Order[A1] = A1
    implicit def _2 : Order[A2] = A2
    implicit def _3 : Order[A3] = A3
    implicit def _4 : Order[A4] = A4
  }
  implicit def tuple5Order[A1, A2, A3, A4, A5](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5]) = new Tuple5Order[A1, A2, A3, A4, A5] {
    implicit def _1 : Order[A1] = A1
    implicit def _2 : Order[A2] = A2
    implicit def _3 : Order[A3] = A3
    implicit def _4 : Order[A4] = A4
    implicit def _5 : Order[A5] = A5
  }
  implicit def tuple6Order[A1, A2, A3, A4, A5, A6](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6]) = new Tuple6Order[A1, A2, A3, A4, A5, A6] {
    implicit def _1 : Order[A1] = A1
    implicit def _2 : Order[A2] = A2
    implicit def _3 : Order[A3] = A3
    implicit def _4 : Order[A4] = A4
    implicit def _5 : Order[A5] = A5
    implicit def _6 : Order[A6] = A6
  }
  implicit def tuple7Order[A1, A2, A3, A4, A5, A6, A7](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7]) = new Tuple7Order[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 : Order[A1] = A1
    implicit def _2 : Order[A2] = A2
    implicit def _3 : Order[A3] = A3
    implicit def _4 : Order[A4] = A4
    implicit def _5 : Order[A5] = A5
    implicit def _6 : Order[A6] = A6
    implicit def _7 : Order[A7] = A7
  }
  implicit def tuple8Order[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8]) = new Tuple8Order[A1, A2, A3, A4, A5, A6, A7, A8] {
    implicit def _1 : Order[A1] = A1
    implicit def _2 : Order[A2] = A2
    implicit def _3 : Order[A3] = A3
    implicit def _4 : Order[A4] = A4
    implicit def _5 : Order[A5] = A5
    implicit def _6 : Order[A6] = A6
    implicit def _7 : Order[A7] = A7
    implicit def _8 : Order[A8] = A8
  }
  implicit def tuple1Monoid[A1](implicit A1: Monoid[A1]) = new Tuple1Monoid[A1] {
      implicit def _1 : Monoid[A1] = A1
  }
  implicit def tuple2Monoid[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]) = new Tuple2Monoid[A1, A2] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
  }
  implicit def tuple3Monoid[A1, A2, A3](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3]) = new Tuple3Monoid[A1, A2, A3] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
  }
  implicit def tuple4Monoid[A1, A2, A3, A4](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4]) = new Tuple4Monoid[A1, A2, A3, A4] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
    implicit def _4 : Monoid[A4] = A4
  }
  implicit def tuple5Monoid[A1, A2, A3, A4, A5](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5]) = new Tuple5Monoid[A1, A2, A3, A4, A5] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
    implicit def _4 : Monoid[A4] = A4
    implicit def _5 : Monoid[A5] = A5
  }
  implicit def tuple6Monoid[A1, A2, A3, A4, A5, A6](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6]) = new Tuple6Monoid[A1, A2, A3, A4, A5, A6] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
    implicit def _4 : Monoid[A4] = A4
    implicit def _5 : Monoid[A5] = A5
    implicit def _6 : Monoid[A6] = A6
  }
  implicit def tuple7Monoid[A1, A2, A3, A4, A5, A6, A7](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6], A7: Monoid[A7]) = new Tuple7Monoid[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
    implicit def _4 : Monoid[A4] = A4
    implicit def _5 : Monoid[A5] = A5
    implicit def _6 : Monoid[A6] = A6
    implicit def _7 : Monoid[A7] = A7
  }
  implicit def tuple8Monoid[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6], A7: Monoid[A7], A8: Monoid[A8]) = new Tuple8Monoid[A1, A2, A3, A4, A5, A6, A7, A8] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
    implicit def _4 : Monoid[A4] = A4
    implicit def _5 : Monoid[A5] = A5
    implicit def _6 : Monoid[A6] = A6
    implicit def _7 : Monoid[A7] = A7
    implicit def _8 : Monoid[A8] = A8
  }

  implicit def tuple1Monad: Monad[Tuple1] = new Tuple1Monad {}
  implicit def tuple2Monad[A1](implicit A1: Monoid[A1]): Monad[({type f[x] = (A1, x)})#f] = new Tuple2Monad[A1] {
    implicit def _1 : Monoid[A1] = A1
  }
  implicit def tuple3Monad[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]): Monad[({type f[x] = (A1, A2, x)})#f] = new Tuple3Monad[A1, A2] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
  }
  implicit def tuple4Monad[A1, A2, A3](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3]): Monad[({type f[x] = (A1, A2, A3, x)})#f] = new Tuple4Monad[A1, A2, A3] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
  }
  implicit def tuple5Monad[A1, A2, A3, A4](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4]): Monad[({type f[x] = (A1, A2, A3, A4, x)})#f] = new Tuple5Monad[A1, A2, A3, A4] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
    implicit def _4 : Monoid[A4] = A4
  }
  implicit def tuple6Monad[A1, A2, A3, A4, A5](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5]): Monad[({type f[x] = (A1, A2, A3, A4, A5, x)})#f] = new Tuple6Monad[A1, A2, A3, A4, A5] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
    implicit def _4 : Monoid[A4] = A4
    implicit def _5 : Monoid[A5] = A5
  }
  implicit def tuple7Monad[A1, A2, A3, A4, A5, A6](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6]): Monad[({type f[x] = (A1, A2, A3, A4, A5, A6, x)})#f] = new Tuple7Monad[A1, A2, A3, A4, A5, A6] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
    implicit def _4 : Monoid[A4] = A4
    implicit def _5 : Monoid[A5] = A5
    implicit def _6 : Monoid[A6] = A6
  }
  implicit def tuple8Monad[A1, A2, A3, A4, A5, A6, A7](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6], A7: Monoid[A7]): Monad[({type f[x] = (A1, A2, A3, A4, A5, A6, A7, x)})#f] = new Tuple8Monad[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 : Monoid[A1] = A1
    implicit def _2 : Monoid[A2] = A2
    implicit def _3 : Monoid[A3] = A3
    implicit def _4 : Monoid[A4] = A4
    implicit def _5 : Monoid[A5] = A5
    implicit def _6 : Monoid[A6] = A6
    implicit def _7 : Monoid[A7] = A7
  }
}

trait TupleInstances extends TupleInstances1

object tuple extends TupleInstances

private[scalaz] trait Tuple1Semigroup[A1] extends Semigroup[Tuple1[A1]] {
  implicit def _1 : Semigroup[A1]
  def append(f1: Tuple1[A1], f2: => Tuple1[A1]): Tuple1[A1] = (
    Tuple1(Semigroup[A1].append(f1._1, f2._1))
    )
}

private[scalaz] trait Tuple2Semigroup[A1, A2] extends Semigroup[(A1, A2)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  def append(f1: (A1, A2), f2: => (A1, A2)): (A1, A2) = (
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2)
    )
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
private[scalaz] trait Tuple4Semigroup[A1, A2, A3, A4] extends Semigroup[(A1, A2, A3, A4)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  def append(f1: (A1, A2, A3, A4), f2: => (A1, A2, A3, A4)): (A1, A2, A3, A4) = (
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2),
    _3.append(f1._3, f2._3),
    _4.append(f1._4, f2._4)
    )
}
private[scalaz] trait Tuple5Semigroup[A1, A2, A3, A4, A5] extends Semigroup[(A1, A2, A3, A4, A5)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  implicit def _5 : Semigroup[A5]
  def append(f1: (A1, A2, A3, A4, A5), f2: => (A1, A2, A3, A4, A5)): (A1, A2, A3, A4, A5) = (
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2),
    _3.append(f1._3, f2._3),
    _4.append(f1._4, f2._4),
    _5.append(f1._5, f2._5)
    )
}
private[scalaz] trait Tuple6Semigroup[A1, A2, A3, A4, A5, A6] extends Semigroup[(A1, A2, A3, A4, A5, A6)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  implicit def _5 : Semigroup[A5]
  implicit def _6 : Semigroup[A6]
  def append(f1: (A1, A2, A3, A4, A5, A6), f2: => (A1, A2, A3, A4, A5, A6)): (A1, A2, A3, A4, A5, A6) = (
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2),
    _3.append(f1._3, f2._3),
    _4.append(f1._4, f2._4),
    _5.append(f1._5, f2._5),
    _6.append(f1._6, f2._6)
    )
}
private[scalaz] trait Tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7] extends Semigroup[(A1, A2, A3, A4, A5, A6, A7)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  implicit def _5 : Semigroup[A5]
  implicit def _6 : Semigroup[A6]
  implicit def _7 : Semigroup[A7]
  def append(f1: (A1, A2, A3, A4, A5, A6, A7), f2: => (A1, A2, A3, A4, A5, A6, A7)): (A1, A2, A3, A4, A5, A6, A7) = (
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2),
    _3.append(f1._3, f2._3),
    _4.append(f1._4, f2._4),
    _5.append(f1._5, f2._5),
    _6.append(f1._6, f2._6),
    _7.append(f1._7, f2._7)
    )
}
private[scalaz] trait Tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8] extends Semigroup[(A1, A2, A3, A4, A5, A6, A7, A8)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  implicit def _5 : Semigroup[A5]
  implicit def _6 : Semigroup[A6]
  implicit def _7 : Semigroup[A7]
  implicit def _8 : Semigroup[A8]
  def append(f1: (A1, A2, A3, A4, A5, A6, A7, A8), f2: => (A1, A2, A3, A4, A5, A6, A7, A8)): (A1, A2, A3, A4, A5, A6, A7, A8) = (
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2),
    _3.append(f1._3, f2._3),
    _4.append(f1._4, f2._4),
    _5.append(f1._5, f2._5),
    _6.append(f1._6, f2._6),
    _7.append(f1._7, f2._7),
    _8.append(f1._8, f2._8)
    )
}
private[scalaz] trait Tuple1Functor extends Functor[Tuple1] {
  def map[A, B](fa: Tuple1[A])(f: (A) => B): Tuple1[B] =
    Tuple1(f(fa._1))
}
private[scalaz] trait Tuple2Functor[A1] extends Functor[({type f[x] = (A1, x)})#f] {
  def map[A, B](fa: (A1, A))(f: A => B): (A1, B) =
    (fa._1, f(fa._2))
}
private[scalaz] trait Tuple3Functor[A1, A2] extends Functor[({type f[x] = (A1, A2, x)})#f] {
  def map[A, B](fa: (A1, A2, A))(f: A => B): (A1, A2, B) =
    (fa._1, fa._2, f(fa._3))
}
private[scalaz] trait Tuple4Functor[A1, A2, A3] extends Functor[({type f[x] = (A1, A2, A3, x)})#f] {
  def map[A, B](fa: (A1, A2, A3, A))(f: A => B): (A1, A2, A3, B) =
    (fa._1, fa._2, fa._3, f(fa._4))
}
private[scalaz] trait Tuple5Functor[A1, A2, A3, A4] extends Functor[({type f[x] = (A1, A2, A3, A4, x)})#f] {
  def map[A, B](fa: (A1, A2, A3, A4, A))(f: A => B): (A1, A2, A3, A4, B) =
    (fa._1, fa._2, fa._3, fa._4, f(fa._5))
}
private[scalaz] trait Tuple6Functor[A1, A2, A3, A4, A5] extends Functor[({type f[x] = (A1, A2, A3, A4, A5, x)})#f] {
  def map[A, B](fa: (A1, A2, A3, A4, A5, A))(f: A => B): (A1, A2, A3, A4, A5, B) =
    (fa._1, fa._2, fa._3, fa._4, fa._5, f(fa._6))
}
private[scalaz] trait Tuple7Functor[A1, A2, A3, A4, A5, A6] extends Functor[({type f[x] = (A1, A2, A3, A4, A5, A6, x)})#f] {
  def map[A, B](fa: (A1, A2, A3, A4, A5, A6, A))(f: A => B): (A1, A2, A3, A4, A5, A6, B) =
    (fa._1, fa._2, fa._3, fa._4, fa._5, fa._6, f(fa._7))
}
private[scalaz] trait Tuple8Functor[A1, A2, A3, A4, A5, A6, A7] extends Functor[({type f[x] = (A1, A2, A3, A4, A5, A6, A7, x)})#f] {
  def map[A, B](fa: (A1, A2, A3, A4, A5, A6, A7, A))(f: A => B): (A1, A2, A3, A4, A5, A6, A7, B) =
    (fa._1, fa._2, fa._3, fa._4, fa._5, fa._6, fa._7, f(fa._8))
}

private[scalaz] trait Tuple1Equal[A1] extends  Equal[Tuple1[A1]] {
  implicit def _1 : Equal[A1]
  def equal(f1: Tuple1[A1], f2: Tuple1[A1]): Boolean = _1.equal(f1._1, f2._1)
}
private[scalaz] trait Tuple2Equal[A1, A2] extends  Equal[(A1, A2)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  def equal(f1: (A1, A2), f2: (A1, A2)): Boolean =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2)
}
private[scalaz] trait Tuple3Equal[A1, A2, A3] extends  Equal[(A1, A2, A3)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  def equal(f1: (A1, A2, A3), f2: (A1, A2, A3)): Boolean =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3)
}
private[scalaz] trait Tuple4Equal[A1, A2, A3, A4] extends  Equal[(A1, A2, A3, A4)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  def equal(f1: (A1, A2, A3, A4), f2: (A1, A2, A3, A4)): Boolean =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4)
}
private[scalaz] trait Tuple5Equal[A1, A2, A3, A4, A5] extends  Equal[(A1, A2, A3, A4, A5)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  implicit def _5 : Equal[A5]
  def equal(f1: (A1, A2, A3, A4, A5), f2: (A1, A2, A3, A4, A5)): Boolean =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4) && _5.equal(f1._5, f2._5)
}
private[scalaz] trait Tuple6Equal[A1, A2, A3, A4, A5, A6] extends  Equal[(A1, A2, A3, A4, A5, A6)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  implicit def _5 : Equal[A5]
  implicit def _6 : Equal[A6]
  def equal(f1: (A1, A2, A3, A4, A5, A6), f2: (A1, A2, A3, A4, A5, A6)): Boolean =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4) && _5.equal(f1._5, f2._5) && _6.equal(f1._6, f2._6)
}
private[scalaz] trait Tuple7Equal[A1, A2, A3, A4, A5, A6, A7] extends  Equal[(A1, A2, A3, A4, A5, A6, A7)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  implicit def _5 : Equal[A5]
  implicit def _6 : Equal[A6]
  implicit def _7 : Equal[A7]
  def equal(f1: (A1, A2, A3, A4, A5, A6, A7), f2: (A1, A2, A3, A4, A5, A6, A7)): Boolean =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4) && _5.equal(f1._5, f2._5) && _6.equal(f1._6, f2._6) && _7.equal(f1._7, f2._7)
}
private[scalaz] trait Tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8] extends  Equal[(A1, A2, A3, A4, A5, A6, A7, A8)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  implicit def _5 : Equal[A5]
  implicit def _6 : Equal[A6]
  implicit def _7 : Equal[A7]
  implicit def _8 : Equal[A8]
  def equal(f1: (A1, A2, A3, A4, A5, A6, A7, A8), f2: (A1, A2, A3, A4, A5, A6, A7, A8)): Boolean =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4) && _5.equal(f1._5, f2._5) && _6.equal(f1._6, f2._6) && _7.equal(f1._7, f2._7) && _8.equal(f1._8, f2._8)
}
private[scalaz] trait Tuple1Show[A1] extends  Show[Tuple1[A1]] {
  implicit def _1 : Show[A1]
  def show(f: Tuple1[A1]): List[Char] =
    "(".toList ::: _1.show(f._1) ::: ")".toList
}
private[scalaz] trait Tuple2Show[A1, A2] extends  Show[(A1, A2)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  def show(f: (A1, A2)): List[Char] =
    "(".toList ::: _1.show(f._1) ::: ",".toList ::: _2.show(f._2) ::: ")".toList
}
private[scalaz] trait Tuple3Show[A1, A2, A3] extends  Show[(A1, A2, A3)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  def show(f: (A1, A2, A3)): List[Char] =
    "(".toList ::: _1.show(f._1) ::: ",".toList ::: _2.show(f._2) ::: ",".toList ::: _3.show(f._3) ::: ")".toList
}
private[scalaz] trait Tuple4Show[A1, A2, A3, A4] extends  Show[(A1, A2, A3, A4)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  def show(f: (A1, A2, A3, A4)): List[Char] =
    "(".toList ::: _1.show(f._1) ::: ",".toList ::: _2.show(f._2) ::: ",".toList ::: _3.show(f._3) ::: ",".toList ::: _4.show(f._4) ::: ")".toList
}
private[scalaz] trait Tuple5Show[A1, A2, A3, A4, A5] extends  Show[(A1, A2, A3, A4, A5)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  implicit def _5 : Show[A5]
  def show(f: (A1, A2, A3, A4, A5)): List[Char] =
    "(".toList ::: _1.show(f._1) ::: ",".toList ::: _2.show(f._2) ::: ",".toList ::: _3.show(f._3) ::: ",".toList ::: _4.show(f._4) ::: ",".toList ::: _5.show(f._5) ::: ")".toList
}
private[scalaz] trait Tuple6Show[A1, A2, A3, A4, A5, A6] extends  Show[(A1, A2, A3, A4, A5, A6)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  implicit def _5 : Show[A5]
  implicit def _6 : Show[A6]
  def show(f: (A1, A2, A3, A4, A5, A6)): List[Char] =
    "(".toList ::: _1.show(f._1) ::: ",".toList ::: _2.show(f._2) ::: ",".toList ::: _3.show(f._3) ::: ",".toList ::: _4.show(f._4) ::: ",".toList ::: _5.show(f._5) ::: ",".toList ::: _6.show(f._6) ::: ")".toList
}
private[scalaz] trait Tuple7Show[A1, A2, A3, A4, A5, A6, A7] extends  Show[(A1, A2, A3, A4, A5, A6, A7)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  implicit def _5 : Show[A5]
  implicit def _6 : Show[A6]
  implicit def _7 : Show[A7]
  def show(f: (A1, A2, A3, A4, A5, A6, A7)): List[Char] =
    "(".toList ::: _1.show(f._1) ::: ",".toList ::: _2.show(f._2) ::: ",".toList ::: _3.show(f._3) ::: ",".toList ::: _4.show(f._4) ::: ",".toList ::: _5.show(f._5) ::: ",".toList ::: _6.show(f._6) ::: ",".toList ::: _7.show(f._7) ::: ")".toList
}
private[scalaz] trait Tuple8Show[A1, A2, A3, A4, A5, A6, A7, A8] extends  Show[(A1, A2, A3, A4, A5, A6, A7, A8)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  implicit def _5 : Show[A5]
  implicit def _6 : Show[A6]
  implicit def _7 : Show[A7]
  implicit def _8 : Show[A8]
  def show(f: (A1, A2, A3, A4, A5, A6, A7, A8)): List[Char] =
    "(".toList ::: _1.show(f._1) ::: ",".toList ::: _2.show(f._2) ::: ",".toList ::: _3.show(f._3) ::: ",".toList ::: _4.show(f._4) ::: ",".toList ::: _5.show(f._5) ::: ",".toList ::: _6.show(f._6) ::: ",".toList ::: _7.show(f._7) ::: ",".toList ::: _8.show(f._8) ::: ")".toList
}

private[scalaz] trait Tuple1Order[A1] extends  Order[Tuple1[A1]] {
  implicit def _1 : Order[A1]
  import Ordering.EQ
  def order(f1: Tuple1[A1], f2: Tuple1[A1]): Ordering = _1.order(f1._1, f2._1)
}
private[scalaz] trait Tuple2Order[A1, A2] extends  Order[(A1, A2)] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  import Ordering.EQ
  def order(f1: (A1, A2), f2: (A1, A2)): Ordering =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2)) match {
      case (EQ, ord) => ord
      case (ord, _) => ord
    }
}
private[scalaz] trait Tuple3Order[A1, A2, A3] extends  Order[(A1, A2, A3)] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  import Ordering.EQ
  def order(f1: (A1, A2, A3), f2: (A1, A2, A3)): Ordering =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3)) match {
      case (EQ, EQ, ord) => ord
      case (EQ, ord, _) => ord
      case (ord, _, _) => ord
    }
}
private[scalaz] trait Tuple4Order[A1, A2, A3, A4] extends  Order[(A1, A2, A3, A4)] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  import Ordering.EQ
  def order(f1: (A1, A2, A3, A4), f2: (A1, A2, A3, A4)): Ordering =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3), _4.order(f1._4, f2._4)) match {
      case (EQ, EQ, EQ, ord) => ord
      case (EQ, EQ, ord, _) => ord
      case (EQ, ord, _, _) => ord
      case (ord, _, _, _) => ord
    }
}
private[scalaz] trait Tuple5Order[A1, A2, A3, A4, A5] extends  Order[(A1, A2, A3, A4, A5)] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  implicit def _5 : Order[A5]
  import Ordering.EQ
  def order(f1: (A1, A2, A3, A4, A5), f2: (A1, A2, A3, A4, A5)): Ordering =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3), _4.order(f1._4, f2._4), _5.order(f1._5, f2._5)) match {
      case (EQ, EQ, EQ, EQ, ord) => ord
      case (EQ, EQ, EQ, ord, _) => ord
      case (EQ, EQ, ord, _, _) => ord
      case (EQ, ord, _, _, _) => ord
      case (ord, _, _, _, _) => ord
    }
}
private[scalaz] trait Tuple6Order[A1, A2, A3, A4, A5, A6] extends  Order[(A1, A2, A3, A4, A5, A6)] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  implicit def _5 : Order[A5]
  implicit def _6 : Order[A6]
  import Ordering.EQ
  def order(f1: (A1, A2, A3, A4, A5, A6), f2: (A1, A2, A3, A4, A5, A6)): Ordering =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3), _4.order(f1._4, f2._4), _5.order(f1._5, f2._5), _6.order(f1._6, f2._6)) match {
      case (EQ, EQ, EQ, EQ, EQ, ord) => ord
      case (EQ, EQ, EQ, EQ, ord, _) => ord
      case (EQ, EQ, EQ, ord, _, _) => ord
      case (EQ, EQ, ord, _, _, _) => ord
      case (EQ, ord, _, _, _, _) => ord
      case (ord, _, _, _, _, _) => ord
    }
}
private[scalaz] trait Tuple7Order[A1, A2, A3, A4, A5, A6, A7] extends  Order[(A1, A2, A3, A4, A5, A6, A7)] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  implicit def _5 : Order[A5]
  implicit def _6 : Order[A6]
  implicit def _7 : Order[A7]
  import Ordering.EQ
  def order(f1: (A1, A2, A3, A4, A5, A6, A7), f2: (A1, A2, A3, A4, A5, A6, A7)): Ordering =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3), _4.order(f1._4, f2._4), _5.order(f1._5, f2._5), _6.order(f1._6, f2._6), _7.order(f1._7, f2._7)) match {
      case (EQ, EQ, EQ, EQ, EQ, EQ, ord) => ord
      case (EQ, EQ, EQ, EQ, EQ, ord, _) => ord
      case (EQ, EQ, EQ, EQ, ord, _, _) => ord
      case (EQ, EQ, EQ, ord, _, _, _) => ord
      case (EQ, EQ, ord, _, _, _, _) => ord
      case (EQ, ord, _, _, _, _, _) => ord
      case (ord, _, _, _, _, _, _) => ord
    }
}
private[scalaz] trait Tuple8Order[A1, A2, A3, A4, A5, A6, A7, A8] extends  Order[(A1, A2, A3, A4, A5, A6, A7, A8)] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  implicit def _5 : Order[A5]
  implicit def _6 : Order[A6]
  implicit def _7 : Order[A7]
  implicit def _8 : Order[A8]
  import Ordering.EQ
  def order(f1: (A1, A2, A3, A4, A5, A6, A7, A8), f2: (A1, A2, A3, A4, A5, A6, A7, A8)): Ordering =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3), _4.order(f1._4, f2._4), _5.order(f1._5, f2._5), _6.order(f1._6, f2._6), _7.order(f1._7, f2._7), _8.order(f1._8, f2._8)) match {
      case (EQ, EQ, EQ, EQ, EQ, EQ, EQ, ord) => ord
      case (EQ, EQ, EQ, EQ, EQ, EQ, ord, _) => ord
      case (EQ, EQ, EQ, EQ, EQ, ord, _, _) => ord
      case (EQ, EQ, EQ, EQ, ord, _, _, _) => ord
      case (EQ, EQ, EQ, ord, _, _, _, _) => ord
      case (EQ, EQ, ord, _, _, _, _, _) => ord
      case (EQ, ord, _, _, _, _, _, _) => ord
      case (ord, _, _, _, _, _, _, _) => ord
    }
}

private[scalaz] trait Tuple1Monoid[A1] extends Tuple1Semigroup[A1] with Monoid[Tuple1[A1]] {
  implicit def _1 : Monoid[A1]
  def zero: Tuple1[A1] = Tuple1(_1.zero)
}
private[scalaz] trait Tuple2Monoid[A1, A2] extends Tuple2Semigroup[A1, A2] with Monoid[(A1, A2)] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  def zero: (A1, A2) = (_1.zero, _2.zero)
}
private[scalaz] trait Tuple3Monoid[A1, A2, A3] extends Tuple3Semigroup[A1, A2, A3] with Monoid[(A1, A2, A3)] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  def zero: (A1, A2, A3) = (_1.zero, _2.zero, _3.zero)
}
private[scalaz] trait Tuple4Monoid[A1, A2, A3, A4] extends Tuple4Semigroup[A1, A2, A3, A4] with Monoid[(A1, A2, A3, A4)] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  def zero: (A1, A2, A3, A4) = (_1.zero, _2.zero, _3.zero, _4.zero)
}
private[scalaz] trait Tuple5Monoid[A1, A2, A3, A4, A5] extends Tuple5Semigroup[A1, A2, A3, A4, A5] with Monoid[(A1, A2, A3, A4, A5)] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  def zero: (A1, A2, A3, A4, A5) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero)
}
private[scalaz] trait Tuple6Monoid[A1, A2, A3, A4, A5, A6] extends Tuple6Semigroup[A1, A2, A3, A4, A5, A6] with Monoid[(A1, A2, A3, A4, A5, A6)] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  def zero: (A1, A2, A3, A4, A5, A6) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero)
}
private[scalaz] trait Tuple7Monoid[A1, A2, A3, A4, A5, A6, A7] extends Tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7] with Monoid[(A1, A2, A3, A4, A5, A6, A7)] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  implicit def _7 : Monoid[A7]
  def zero: (A1, A2, A3, A4, A5, A6, A7) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero, _7.zero)
}
private[scalaz] trait Tuple8Monoid[A1, A2, A3, A4, A5, A6, A7, A8] extends Tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8] with Monoid[(A1, A2, A3, A4, A5, A6, A7, A8)] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  implicit def _7 : Monoid[A7]
  implicit def _8 : Monoid[A8]
  def zero: (A1, A2, A3, A4, A5, A6, A7, A8) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero, _7.zero, _8.zero)
}

private[scalaz] trait Tuple1Monad extends Monad[Tuple1] {
  def bind[A, B](fa: Tuple1[A])(f: A => Tuple1[B]): Tuple1[B] = f(fa._1)
  def point[A](a: => A): Tuple1[A] = Tuple1(a)
}
private[scalaz] trait Tuple2Monad[A1] extends Monad[({type f[x] = (A1, x)})#f] {
  implicit def _1 : Monoid[A1]
  def bind[A, B](fa: (A1, A))(f: A => (A1, B)): (A1, B) = f(fa._2)
  def point[A](a: => A): (A1, A) = (_1.zero, a)
}
private[scalaz] trait Tuple3Monad[A1, A2] extends Monad[({type f[x] = (A1, A2, x)})#f] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  def bind[A, B](fa: (A1, A2, A))(f: A => (A1, A2, B)): (A1, A2, B) = f(fa._3)
  def point[A](a: => A): (A1, A2, A) = (_1.zero, _2.zero, a)
}
private[scalaz] trait Tuple4Monad[A1, A2, A3] extends Monad[({type f[x] = (A1, A2, A3, x)})#f] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  def bind[A, B](fa: (A1, A2, A3, A))(f: A => (A1, A2, A3, B)): (A1, A2, A3, B) = f(fa._4)
  def point[A](a: => A): (A1, A2, A3, A) = (_1.zero, _2.zero, _3.zero, a)
}
private[scalaz] trait Tuple5Monad[A1, A2, A3, A4] extends Monad[({type f[x] = (A1, A2, A3, A4, x)})#f] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  def bind[A, B](fa: (A1, A2, A3, A4, A))(f: A => (A1, A2, A3, A4, B)): (A1, A2, A3, A4, B) = f(fa._5)
  def point[A](a: => A): (A1, A2, A3, A4, A) = (_1.zero, _2.zero, _3.zero, _4.zero, a)
}
private[scalaz] trait Tuple6Monad[A1, A2, A3, A4, A5] extends Monad[({type f[x] = (A1, A2, A3, A4, A5, x)})#f] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  def bind[A, B](fa: (A1, A2, A3, A4, A5, A))(f: A => (A1, A2, A3, A4, A5, B)): (A1, A2, A3, A4, A5, B) = f(fa._6)
  def point[A](a: => A): (A1, A2, A3, A4, A5, A) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, a)
}
private[scalaz] trait Tuple7Monad[A1, A2, A3, A4, A5, A6] extends Monad[({type f[x] = (A1, A2, A3, A4, A5, A6, x)})#f] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  def bind[A, B](fa: (A1, A2, A3, A4, A5, A6, A))(f: A => (A1, A2, A3, A4, A5, A6, B)): (A1, A2, A3, A4, A5, A6, B) = f(fa._7)
  def point[A](a: => A): (A1, A2, A3, A4, A5, A6, A) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero, a)
}
private[scalaz] trait Tuple8Monad[A1, A2, A3, A4, A5, A6, A7] extends Monad[({type f[x] = (A1, A2, A3, A4, A5, A6, A7, x)})#f] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  implicit def _7 : Monoid[A7]
  def bind[A, B](fa: (A1, A2, A3, A4, A5, A6, A7, A))(f: A => (A1, A2, A3, A4, A5, A6, A7, B)): (A1, A2, A3, A4, A5, A6, A7, B) = f(fa._8)
  def point[A](a: => A): (A1, A2, A3, A4, A5, A6, A7, A) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero, _7.zero, a)
}

