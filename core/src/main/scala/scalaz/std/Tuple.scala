package scalaz
package std

trait TupleInstances0 {
  implicit def tuple2Bitraverse[A1, A2] = new Bitraverse[Tuple2] {
    override def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D) =
      (f(fab._1), g(fab._2))
    def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: (A, B))(f: A => G[C], g: B => G[D]) =
      Applicative[G].apply2(f(fab._1), g(fab._2))((_, _))
  }

  implicit def tuple1Semigroup[A1](implicit A1: Semigroup[A1]) = new Tuple1Semigroup[A1] {
    implicit def _1: Semigroup[A1] = A1
  }
  implicit def tuple2Semigroup[A1, A2](implicit A1: Semigroup[A1], A2: Semigroup[A2]) = new Tuple2Semigroup[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }
  implicit def tuple3Semigroup[A1, A2, A3](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3]) = new Tuple3Semigroup[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }
  implicit def tuple4Semigroup[A1, A2, A3, A4](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4]) = new Tuple4Semigroup[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }
  implicit def tuple5Semigroup[A1, A2, A3, A4, A5](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5]) = new Tuple5Semigroup[A1, A2, A3, A4, A5] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
  }
  implicit def tuple6Semigroup[A1, A2, A3, A4, A5, A6](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5], A6: Semigroup[A6]) = new Tuple6Semigroup[A1, A2, A3, A4, A5, A6] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
  }
  implicit def tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5], A6: Semigroup[A6], A7: Semigroup[A7]) = new Tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
  }
  implicit def tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5], A6: Semigroup[A6], A7: Semigroup[A7], A8: Semigroup[A8]) = new Tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
    implicit def _8 = A8
  }
  /** `Tuple1[A]` is isomorphic to `Id[X]` */
  implicit def tuple1Instance: Traverse[Tuple1] with Monad[Tuple1] with Comonad[Tuple1] = new Tuple1Monad with Tuple1Functor with Comonad[Tuple1] {
    override def cojoin[A](a: Tuple1[A]) = Tuple1(a)
    def copoint[A](p: Tuple1[A]) = p._1
    def cobind[A, B](fa: Tuple1[A])(f: Tuple1[A] => B) = Tuple1(f(fa))
  }

  /** Product functor and comonad */
  implicit def tuple2Instance[A1]: Traverse[({type f[x] = (A1, x)})#f] with Comonad[({type f[x] = (A1, x)})#f] = new Tuple2Functor[A1] with Comonad[({type f[x] = (A1, x)})#f] {
    override def cojoin[A](a: (A1, A)) = (a._1, a)
    def copoint[A](p: (A1, A)) = p._2
    def cobind[A, B](fa: (A1, A))(f: ((A1, A)) => B) = (fa._1, f(fa))
  }

  implicit def tuple3Functor[A1, A2]: Traverse[({type f[x] = (A1, A2, x)})#f] = new Tuple3Functor[A1, A2] {}
  implicit def tuple4Functor[A1, A2, A3]: Traverse[({type f[x] = (A1, A2, A3, x)})#f] = new Tuple4Functor[A1, A2, A3] {}
  implicit def tuple5Functor[A1, A2, A3, A4]: Traverse[({type f[x] = (A1, A2, A3, A4, x)})#f] = new Tuple5Functor[A1, A2, A3, A4] {}
  implicit def tuple6Functor[A1, A2, A3, A4, A5]: Traverse[({type f[x] = (A1, A2, A3, A4, A5, x)})#f] = new Tuple6Functor[A1, A2, A3, A4, A5] {}
  implicit def tuple7Functor[A1, A2, A3, A4, A5, A6]: Traverse[({type f[x] = (A1, A2, A3, A4, A5, A6, x)})#f] = new Tuple7Functor[A1, A2, A3, A4, A5, A6] {}
  implicit def tuple8Functor[A1, A2, A3, A4, A5, A6, A7]: Traverse[({type f[x] = (A1, A2, A3, A4, A5, A6, A7, x)})#f] = new Tuple8Functor[A1, A2, A3, A4, A5, A6, A7] {}

  implicit def tuple1Equal[A1](implicit A1: Equal[A1]) = new Tuple1Equal[A1] {
    implicit def _1 = A1
  }
  implicit def tuple2Equal[A1, A2](implicit A1: Equal[A1], A2: Equal[A2]) = new Tuple2Equal[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }
  implicit def tuple3Equal[A1, A2, A3](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3]) = new Tuple3Equal[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }
  implicit def tuple4Equal[A1, A2, A3, A4](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4]) = new Tuple4Equal[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }
  implicit def tuple5Equal[A1, A2, A3, A4, A5](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5]) = new Tuple5Equal[A1, A2, A3, A4, A5] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
  }
  implicit def tuple6Equal[A1, A2, A3, A4, A5, A6](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5], A6: Equal[A6]) = new Tuple6Equal[A1, A2, A3, A4, A5, A6] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
  }
  implicit def tuple7Equal[A1, A2, A3, A4, A5, A6, A7](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5], A6: Equal[A6], A7: Equal[A7]) = new Tuple7Equal[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
  }
  implicit def tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5], A6: Equal[A6], A7: Equal[A7], A8: Equal[A8]) = new Tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
    implicit def _8 = A8
  }
}
trait TupleInstances1 extends TupleInstances0 {

  implicit def tuple1Show[A1](implicit A1: Show[A1]) = new Tuple1Show[A1] {
    implicit def _1 = A1
  }
  implicit def tuple2Show[A1, A2](implicit A1: Show[A1], A2: Show[A2]) = new Tuple2Show[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }
  implicit def tuple3Show[A1, A2, A3](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3]) = new Tuple3Show[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }
  implicit def tuple4Show[A1, A2, A3, A4](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4]) = new Tuple4Show[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }
  implicit def tuple5Show[A1, A2, A3, A4, A5](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5]) = new Tuple5Show[A1, A2, A3, A4, A5] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
  }
  implicit def tuple6Show[A1, A2, A3, A4, A5, A6](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5], A6: Show[A6]) = new Tuple6Show[A1, A2, A3, A4, A5, A6] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
  }
  implicit def tuple7Show[A1, A2, A3, A4, A5, A6, A7](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5], A6: Show[A6], A7: Show[A7]) = new Tuple7Show[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
  }
  implicit def tuple8Show[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5], A6: Show[A6], A7: Show[A7], A8: Show[A8]) = new Tuple8Show[A1, A2, A3, A4, A5, A6, A7, A8] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
    implicit def _8 = A8
  }

  implicit def tuple1Order[A1](implicit A1: Order[A1]) = new Tuple1Order[A1] {
    implicit def _1 = A1
  }
  implicit def tuple2Order[A1, A2](implicit A1: Order[A1], A2: Order[A2]) = new Tuple2Order[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }
  implicit def tuple3Order[A1, A2, A3](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3]) = new Tuple3Order[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }
  implicit def tuple4Order[A1, A2, A3, A4](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4]) = new Tuple4Order[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }
  implicit def tuple5Order[A1, A2, A3, A4, A5](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5]) = new Tuple5Order[A1, A2, A3, A4, A5] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
  }
  implicit def tuple6Order[A1, A2, A3, A4, A5, A6](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6]) = new Tuple6Order[A1, A2, A3, A4, A5, A6] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
  }
  implicit def tuple7Order[A1, A2, A3, A4, A5, A6, A7](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7]) = new Tuple7Order[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
  }
  implicit def tuple8Order[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8]) = new Tuple8Order[A1, A2, A3, A4, A5, A6, A7, A8] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
    implicit def _8 = A8
  }
  implicit def tuple1Monoid[A1](implicit A1: Monoid[A1]) = new Tuple1Monoid[A1] {
      implicit def _1 = A1
  }
  implicit def tuple2Monoid[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]) = new Tuple2Monoid[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }
  implicit def tuple3Monoid[A1, A2, A3](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3]) = new Tuple3Monoid[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }
  implicit def tuple4Monoid[A1, A2, A3, A4](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4]) = new Tuple4Monoid[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }
  implicit def tuple5Monoid[A1, A2, A3, A4, A5](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5]) = new Tuple5Monoid[A1, A2, A3, A4, A5] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
  }
  implicit def tuple6Monoid[A1, A2, A3, A4, A5, A6](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6]) = new Tuple6Monoid[A1, A2, A3, A4, A5, A6] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
  }
  implicit def tuple7Monoid[A1, A2, A3, A4, A5, A6, A7](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6], A7: Monoid[A7]) = new Tuple7Monoid[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
  }
  implicit def tuple8Monoid[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6], A7: Monoid[A7], A8: Monoid[A8]) = new Tuple8Monoid[A1, A2, A3, A4, A5, A6, A7, A8] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
    implicit def _8 = A8
  }

  implicit def tuple1Cozip: Cozip[Tuple1] = new Tuple1Cozip {}
  implicit def tuple2Cozip[A1]: Cozip[({type f[x] = (A1, x)})#f] = new Tuple2Cozip[A1] {}
  implicit def tuple3Cozip[A1, A2]: Cozip[({type f[x] = (A1, A2, x)})#f] = new Tuple3Cozip[A1, A2] {}
  implicit def tuple4Cozip[A1, A2, A3]: Cozip[({type f[x] = (A1, A2, A3, x)})#f] = new Tuple4Cozip[A1, A2, A3] {}
  implicit def tuple5Cozip[A1, A2, A3, A4]: Cozip[({type f[x] = (A1, A2, A3, A4, x)})#f] = new Tuple5Cozip[A1, A2, A3, A4] {}
  implicit def tuple6Cozip[A1, A2, A3, A4, A5]: Cozip[({type f[x] = (A1, A2, A3, A4, A5, x)})#f] = new Tuple6Cozip[A1, A2, A3, A4, A5] {}
  implicit def tuple7Cozip[A1, A2, A3, A4, A5, A6]: Cozip[({type f[x] = (A1, A2, A3, A4, A5, A6, x)})#f] = new Tuple7Cozip[A1, A2, A3, A4, A5, A6] {}
  implicit def tuple8Cozip[A1, A2, A3, A4, A5, A6, A7]: Cozip[({type f[x] = (A1, A2, A3, A4, A5, A6, A7, x)})#f] = new Tuple8Cozip[A1, A2, A3, A4, A5, A6, A7] {}

  implicit def tuple2Monad[A1](implicit A1: Monoid[A1]): Monad[({type f[x] = (A1, x)})#f] = new Tuple2Monad[A1] {
    implicit def _1 = A1
  }
  implicit def tuple3Monad[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]): Monad[({type f[x] = (A1, A2, x)})#f] = new Tuple3Monad[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }
  implicit def tuple4Monad[A1, A2, A3](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3]): Monad[({type f[x] = (A1, A2, A3, x)})#f] = new Tuple4Monad[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }
  implicit def tuple5Monad[A1, A2, A3, A4](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4]): Monad[({type f[x] = (A1, A2, A3, A4, x)})#f] = new Tuple5Monad[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }
  implicit def tuple6Monad[A1, A2, A3, A4, A5](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5]): Monad[({type f[x] = (A1, A2, A3, A4, A5, x)})#f] = new Tuple6Monad[A1, A2, A3, A4, A5] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
  }
  implicit def tuple7Monad[A1, A2, A3, A4, A5, A6](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6]): Monad[({type f[x] = (A1, A2, A3, A4, A5, A6, x)})#f] = new Tuple7Monad[A1, A2, A3, A4, A5, A6] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
  }
  implicit def tuple8Monad[A1, A2, A3, A4, A5, A6, A7](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6], A7: Monoid[A7]): Monad[({type f[x] = (A1, A2, A3, A4, A5, A6, A7, x)})#f] = new Tuple8Monad[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
  }

}

trait TupleInstances extends TupleInstances1

object tuple extends TupleInstances {
  object tupleSyntax extends scalaz.syntax.std.ToTupleOps
}

private[scalaz] trait Tuple1Semigroup[A1] extends Semigroup[Tuple1[A1]] {
  implicit def _1 : Semigroup[A1]
  def append(f1: Tuple1[A1], f2: => Tuple1[A1]) = (
    Tuple1(Semigroup[A1].append(f1._1, f2._1))
    )
}

private[scalaz] trait Tuple2Semigroup[A1, A2] extends Semigroup[(A1, A2)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  def append(f1: (A1, A2), f2: => (A1, A2)) = (
    _1.append(f1._1, f2._1),
    _2.append(f1._2, f2._2)
    )
}
private[scalaz] trait Tuple3Semigroup[A1, A2, A3] extends Semigroup[(A1, A2, A3)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  def append(f1: (A1, A2, A3), f2: => (A1, A2, A3)) = (
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
  def append(f1: (A1, A2, A3, A4), f2: => (A1, A2, A3, A4)) = (
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
  def append(f1: (A1, A2, A3, A4, A5), f2: => (A1, A2, A3, A4, A5)) = (
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
  def append(f1: (A1, A2, A3, A4, A5, A6), f2: => (A1, A2, A3, A4, A5, A6)) = (
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
  def append(f1: (A1, A2, A3, A4, A5, A6, A7), f2: => (A1, A2, A3, A4, A5, A6, A7)) = (
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
  def append(f1: (A1, A2, A3, A4, A5, A6, A7, A8), f2: => (A1, A2, A3, A4, A5, A6, A7, A8)) = (
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
private[scalaz] trait Tuple1Functor extends Traverse[Tuple1] {
  override def map[A, B](fa: Tuple1[A])(f: A => B) =
    Tuple1(f(fa._1))
  def traverseImpl[G[_], A, B](fa: Tuple1[A])(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(f(fa._1))(Tuple1.apply)
}
private[scalaz] trait Tuple2Functor[A1] extends Traverse[({type f[x] = (A1, x)})#f] {
  override def map[A, B](fa: (A1, A))(f: A => B) =
    (fa._1, f(fa._2))
  def traverseImpl[G[_], A, B](fa: (A1, A))(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(f(fa._2))((fa._1, _))
}
private[scalaz] trait Tuple3Functor[A1, A2] extends Traverse[({type f[x] = (A1, A2, x)})#f] {
  override def map[A, B](fa: (A1, A2, A))(f: A => B) =
    (fa._1, fa._2, f(fa._3))
  def traverseImpl[G[_], A, B](fa: (A1, A2, A))(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(f(fa._3))((fa._1, fa._2, _))
}
private[scalaz] trait Tuple4Functor[A1, A2, A3] extends Traverse[({type f[x] = (A1, A2, A3, x)})#f] {
  override def map[A, B](fa: (A1, A2, A3, A))(f: A => B) =
    (fa._1, fa._2, fa._3, f(fa._4))
  def traverseImpl[G[_], A, B](fa: (A1, A2, A3, A))(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(f(fa._4))((fa._1, fa._2, fa._3, _))
}
private[scalaz] trait Tuple5Functor[A1, A2, A3, A4] extends Traverse[({type f[x] = (A1, A2, A3, A4, x)})#f] {
  override def map[A, B](fa: (A1, A2, A3, A4, A))(f: A => B) =
    (fa._1, fa._2, fa._3, fa._4, f(fa._5))
  def traverseImpl[G[_], A, B](fa: (A1, A2, A3, A4, A))(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(f(fa._5))((fa._1, fa._2, fa._3, fa._4, _))
}
private[scalaz] trait Tuple6Functor[A1, A2, A3, A4, A5] extends Traverse[({type f[x] = (A1, A2, A3, A4, A5, x)})#f] {
  override def map[A, B](fa: (A1, A2, A3, A4, A5, A))(f: A => B) =
    (fa._1, fa._2, fa._3, fa._4, fa._5, f(fa._6))
  def traverseImpl[G[_], A, B](fa: (A1, A2, A3, A4, A5, A))(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(f(fa._6))((fa._1, fa._2, fa._3, fa._4, fa._5, _))
}
private[scalaz] trait Tuple7Functor[A1, A2, A3, A4, A5, A6] extends Traverse[({type f[x] = (A1, A2, A3, A4, A5, A6, x)})#f] {
  override def map[A, B](fa: (A1, A2, A3, A4, A5, A6, A))(f: A => B) =
    (fa._1, fa._2, fa._3, fa._4, fa._5, fa._6, f(fa._7))
  def traverseImpl[G[_], A, B](fa: (A1, A2, A3, A4, A5, A6, A))(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(f(fa._7))((fa._1, fa._2, fa._3, fa._4, fa._5, fa._6, _))
}
private[scalaz] trait Tuple8Functor[A1, A2, A3, A4, A5, A6, A7] extends Traverse[({type f[x] = (A1, A2, A3, A4, A5, A6, A7, x)})#f] {
  override def map[A, B](fa: (A1, A2, A3, A4, A5, A6, A7, A))(f: A => B) =
    (fa._1, fa._2, fa._3, fa._4, fa._5, fa._6, fa._7, f(fa._8))
  def traverseImpl[G[_], A, B](fa: (A1, A2, A3, A4, A5, A6, A7, A))(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(f(fa._8))((fa._1, fa._2, fa._3, fa._4, fa._5, fa._6, fa._7, _))
}

private[scalaz] trait Tuple1Cozip extends Cozip[Tuple1] {
  override def cozip[A, B](x: Tuple1[A \/ B]) =
    x._1.bimap(Tuple1(_), Tuple1(_))
}
private[scalaz] trait Tuple2Cozip[A1] extends Cozip[({type f[x] = (A1, x)})#f] {
  override def cozip[A, B](x: (A1, A \/ B)) =
    x._2.bimap((x._1, _), (x._1, _))
}
private[scalaz] trait Tuple3Cozip[A1, A2] extends Cozip[({type f[x] = (A1, A2, x)})#f] {
  override def cozip[A, B](x: (A1, A2, A \/ B)) =
    x._3.bimap((x._1, x._2, _), (x._1, x._2, _))
}
private[scalaz] trait Tuple4Cozip[A1, A2, A3] extends Cozip[({type f[x] = (A1, A2, A3, x)})#f] {
  override def cozip[A, B](x: (A1, A2, A3, A \/ B)) =
    x._4.bimap((x._1, x._2, x._3, _), (x._1, x._2, x._3, _))
}
private[scalaz] trait Tuple5Cozip[A1, A2, A3, A4] extends Cozip[({type f[x] = (A1, A2, A3, A4, x)})#f] {
  override def cozip[A, B](x: (A1, A2, A3, A4, A \/ B)) =
    x._5.bimap((x._1, x._2, x._3, x._4, _), (x._1, x._2, x._3, x._4, _))
}
private[scalaz] trait Tuple6Cozip[A1, A2, A3, A4, A5] extends Cozip[({type f[x] = (A1, A2, A3, A4, A5, x)})#f] {
  override def cozip[A, B](x: (A1, A2, A3, A4, A5, A \/ B)) =
    x._6.bimap((x._1, x._2, x._3, x._4, x._5, _), (x._1, x._2, x._3, x._4, x._5, _))
}
private[scalaz] trait Tuple7Cozip[A1, A2, A3, A4, A5, A6] extends Cozip[({type f[x] = (A1, A2, A3, A4, A5, A6, x)})#f] {
  override def cozip[A, B](x: (A1, A2, A3, A4, A5, A6, A \/ B)) =
    x._7.bimap((x._1, x._2, x._3, x._4, x._5, x._6, _), (x._1, x._2, x._3, x._4, x._5, x._6, _))
}
private[scalaz] trait Tuple8Cozip[A1, A2, A3, A4, A5, A6, A7] extends Cozip[({type f[x] = (A1, A2, A3, A4, A5, A6, A7, x)})#f] {
  override def cozip[A, B](x: (A1, A2, A3, A4, A5, A6, A7, A \/ B)) =
    x._8.bimap((x._1, x._2, x._3, x._4, x._5, x._6, x._7, _), (x._1, x._2, x._3, x._4, x._5, x._6, x._7, _))
}

private[scalaz] trait Tuple1Equal[A1] extends Equal[Tuple1[A1]] {
  implicit def _1 : Equal[A1]
  override def equal(f1: Tuple1[A1], f2: Tuple1[A1]) = _1.equal(f1._1, f2._1)
  override val equalIsNatural: Boolean = _1.equalIsNatural
}
private[scalaz] trait Tuple2Equal[A1, A2] extends Equal[(A1, A2)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  override def equal(f1: (A1, A2), f2: (A1, A2)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural
}
private[scalaz] trait Tuple3Equal[A1, A2, A3] extends Equal[(A1, A2, A3)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  override def equal(f1: (A1, A2, A3), f2: (A1, A2, A3)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural && _3.equalIsNatural
}
private[scalaz] trait Tuple4Equal[A1, A2, A3, A4] extends Equal[(A1, A2, A3, A4)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  override def equal(f1: (A1, A2, A3, A4), f2: (A1, A2, A3, A4)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural && _3.equalIsNatural && _4.equalIsNatural
}
private[scalaz] trait Tuple5Equal[A1, A2, A3, A4, A5] extends Equal[(A1, A2, A3, A4, A5)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  implicit def _5 : Equal[A5]
  override def equal(f1: (A1, A2, A3, A4, A5), f2: (A1, A2, A3, A4, A5)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4) && _5.equal(f1._5, f2._5)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural && _3.equalIsNatural && _4.equalIsNatural && _5.equalIsNatural
}
private[scalaz] trait Tuple6Equal[A1, A2, A3, A4, A5, A6] extends Equal[(A1, A2, A3, A4, A5, A6)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  implicit def _5 : Equal[A5]
  implicit def _6 : Equal[A6]
  override def equal(f1: (A1, A2, A3, A4, A5, A6), f2: (A1, A2, A3, A4, A5, A6)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4) && _5.equal(f1._5, f2._5) && _6.equal(f1._6, f2._6)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural && _3.equalIsNatural && _4.equalIsNatural && _5.equalIsNatural && _6.equalIsNatural
}
private[scalaz] trait Tuple7Equal[A1, A2, A3, A4, A5, A6, A7] extends Equal[(A1, A2, A3, A4, A5, A6, A7)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  implicit def _5 : Equal[A5]
  implicit def _6 : Equal[A6]
  implicit def _7 : Equal[A7]
  override def equal(f1: (A1, A2, A3, A4, A5, A6, A7), f2: (A1, A2, A3, A4, A5, A6, A7)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4) && _5.equal(f1._5, f2._5) && _6.equal(f1._6, f2._6) && _7.equal(f1._7, f2._7)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural && _3.equalIsNatural && _4.equalIsNatural && _5.equalIsNatural && _6.equalIsNatural && _7.equalIsNatural
}
private[scalaz] trait Tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8] extends Equal[(A1, A2, A3, A4, A5, A6, A7, A8)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  implicit def _5 : Equal[A5]
  implicit def _6 : Equal[A6]
  implicit def _7 : Equal[A7]
  implicit def _8 : Equal[A8]
  override def equal(f1: (A1, A2, A3, A4, A5, A6, A7, A8), f2: (A1, A2, A3, A4, A5, A6, A7, A8)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4) && _5.equal(f1._5, f2._5) && _6.equal(f1._6, f2._6) && _7.equal(f1._7, f2._7) && _8.equal(f1._8, f2._8)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural && _3.equalIsNatural && _4.equalIsNatural && _5.equalIsNatural && _6.equalIsNatural && _7.equalIsNatural && _8.equalIsNatural
}
private[scalaz] trait Tuple1Show[A1] extends Show[Tuple1[A1]] {
  implicit def _1 : Show[A1]
  override def show(f: Tuple1[A1]) =
    Cord("(", _1.show(f._1), ")")
}
private[scalaz] trait Tuple2Show[A1, A2] extends Show[(A1, A2)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  override def show(f: (A1, A2)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ")")
}
private[scalaz] trait Tuple3Show[A1, A2, A3] extends Show[(A1, A2, A3)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  override def show(f: (A1, A2, A3)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ")")
}
private[scalaz] trait Tuple4Show[A1, A2, A3, A4] extends Show[(A1, A2, A3, A4)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  override def show(f: (A1, A2, A3, A4)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ",", _4.show(f._4), ")")
}
private[scalaz] trait Tuple5Show[A1, A2, A3, A4, A5] extends Show[(A1, A2, A3, A4, A5)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  implicit def _5 : Show[A5]
  override def show(f: (A1, A2, A3, A4, A5)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ",", _4.show(f._4), ",", _5.show(f._5), ")")
}
private[scalaz] trait Tuple6Show[A1, A2, A3, A4, A5, A6] extends Show[(A1, A2, A3, A4, A5, A6)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  implicit def _5 : Show[A5]
  implicit def _6 : Show[A6]
  override def show(f: (A1, A2, A3, A4, A5, A6)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ",", _4.show(f._4), ",", _5.show(f._5), ",", _6.show(f._6), ")")
}
private[scalaz] trait Tuple7Show[A1, A2, A3, A4, A5, A6, A7] extends Show[(A1, A2, A3, A4, A5, A6, A7)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  implicit def _5 : Show[A5]
  implicit def _6 : Show[A6]
  implicit def _7 : Show[A7]
  override def show(f: (A1, A2, A3, A4, A5, A6, A7)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ",", _4.show(f._4), ",", _5.show(f._5), ",", _6.show(f._6), ",", _7.show(f._7), ")")
}
private[scalaz] trait Tuple8Show[A1, A2, A3, A4, A5, A6, A7, A8] extends Show[(A1, A2, A3, A4, A5, A6, A7, A8)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  implicit def _5 : Show[A5]
  implicit def _6 : Show[A6]
  implicit def _7 : Show[A7]
  implicit def _8 : Show[A8]
  override def show(f: (A1, A2, A3, A4, A5, A6, A7, A8)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ",", _4.show(f._4), ",", _5.show(f._5), ",", _6.show(f._6), ",", _7.show(f._7), ",", _8.show(f._8), ")")
}

private[scalaz] trait Tuple1Order[A1] extends Order[Tuple1[A1]] with Tuple1Equal[A1] {
  implicit def _1 : Order[A1]
  def order(f1: Tuple1[A1], f2: Tuple1[A1]) = _1.order(f1._1, f2._1)
}
private[scalaz] trait Tuple2Order[A1, A2] extends Order[(A1, A2)] with Tuple2Equal[A1, A2] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  import Ordering.EQ
  def order(f1: (A1, A2), f2: (A1, A2)) =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2)) match {
      case (EQ, ord) => ord
      case (ord, _) => ord
    }
}
private[scalaz] trait Tuple3Order[A1, A2, A3] extends Order[(A1, A2, A3)] with Tuple3Equal[A1, A2, A3]{
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  import Ordering.EQ
  def order(f1: (A1, A2, A3), f2: (A1, A2, A3)) =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3)) match {
      case (EQ, EQ, ord) => ord
      case (EQ, ord, _) => ord
      case (ord, _, _) => ord
    }
}
private[scalaz] trait Tuple4Order[A1, A2, A3, A4] extends Order[(A1, A2, A3, A4)] with Tuple4Equal[A1, A2, A3, A4]{
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  import Ordering.EQ
  def order(f1: (A1, A2, A3, A4), f2: (A1, A2, A3, A4)) =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3), _4.order(f1._4, f2._4)) match {
      case (EQ, EQ, EQ, ord) => ord
      case (EQ, EQ, ord, _) => ord
      case (EQ, ord, _, _) => ord
      case (ord, _, _, _) => ord
    }
}
private[scalaz] trait Tuple5Order[A1, A2, A3, A4, A5] extends Order[(A1, A2, A3, A4, A5)] with Tuple5Equal[A1, A2, A3, A4, A5] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  implicit def _5 : Order[A5]
  import Ordering.EQ
  def order(f1: (A1, A2, A3, A4, A5), f2: (A1, A2, A3, A4, A5)) =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3), _4.order(f1._4, f2._4), _5.order(f1._5, f2._5)) match {
      case (EQ, EQ, EQ, EQ, ord) => ord
      case (EQ, EQ, EQ, ord, _) => ord
      case (EQ, EQ, ord, _, _) => ord
      case (EQ, ord, _, _, _) => ord
      case (ord, _, _, _, _) => ord
    }
}
private[scalaz] trait Tuple6Order[A1, A2, A3, A4, A5, A6] extends Order[(A1, A2, A3, A4, A5, A6)] with Tuple6Equal[A1, A2, A3, A4, A5, A6] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  implicit def _5 : Order[A5]
  implicit def _6 : Order[A6]
  import Ordering.EQ
  def order(f1: (A1, A2, A3, A4, A5, A6), f2: (A1, A2, A3, A4, A5, A6)) =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2), _3.order(f1._3, f2._3), _4.order(f1._4, f2._4), _5.order(f1._5, f2._5), _6.order(f1._6, f2._6)) match {
      case (EQ, EQ, EQ, EQ, EQ, ord) => ord
      case (EQ, EQ, EQ, EQ, ord, _) => ord
      case (EQ, EQ, EQ, ord, _, _) => ord
      case (EQ, EQ, ord, _, _, _) => ord
      case (EQ, ord, _, _, _, _) => ord
      case (ord, _, _, _, _, _) => ord
    }
}
private[scalaz] trait Tuple7Order[A1, A2, A3, A4, A5, A6, A7] extends Order[(A1, A2, A3, A4, A5, A6, A7)] with Tuple7Equal[A1, A2, A3, A4, A5, A6, A7]{
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  implicit def _5 : Order[A5]
  implicit def _6 : Order[A6]
  implicit def _7 : Order[A7]
  import Ordering.EQ
  def order(f1: (A1, A2, A3, A4, A5, A6, A7), f2: (A1, A2, A3, A4, A5, A6, A7)) =
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
private[scalaz] trait Tuple8Order[A1, A2, A3, A4, A5, A6, A7, A8] extends Order[(A1, A2, A3, A4, A5, A6, A7, A8)] with Tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  implicit def _3 : Order[A3]
  implicit def _4 : Order[A4]
  implicit def _5 : Order[A5]
  implicit def _6 : Order[A6]
  implicit def _7 : Order[A7]
  implicit def _8 : Order[A8]
  import Ordering.EQ
  def order(f1: (A1, A2, A3, A4, A5, A6, A7, A8), f2: (A1, A2, A3, A4, A5, A6, A7, A8)) =
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

private[scalaz] trait Tuple1Monoid[A1] extends Monoid[Tuple1[A1]] with Tuple1Semigroup[A1] {
  implicit def _1 : Monoid[A1]
  def zero: Tuple1[A1] = Tuple1(_1.zero)
}
private[scalaz] trait Tuple2Monoid[A1, A2] extends Monoid[(A1, A2)] with Tuple2Semigroup[A1, A2] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  def zero: (A1, A2) = (_1.zero, _2.zero)
}
private[scalaz] trait Tuple3Monoid[A1, A2, A3] extends Monoid[(A1, A2, A3)] with Tuple3Semigroup[A1, A2, A3] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  def zero: (A1, A2, A3) = (_1.zero, _2.zero, _3.zero)
}
private[scalaz] trait Tuple4Monoid[A1, A2, A3, A4] extends Monoid[(A1, A2, A3, A4)] with Tuple4Semigroup[A1, A2, A3, A4] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  def zero: (A1, A2, A3, A4) = (_1.zero, _2.zero, _3.zero, _4.zero)
}
private[scalaz] trait Tuple5Monoid[A1, A2, A3, A4, A5] extends Monoid[(A1, A2, A3, A4, A5)] with Tuple5Semigroup[A1, A2, A3, A4, A5] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  def zero: (A1, A2, A3, A4, A5) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero)
}
private[scalaz] trait Tuple6Monoid[A1, A2, A3, A4, A5, A6] extends Monoid[(A1, A2, A3, A4, A5, A6)] with Tuple6Semigroup[A1, A2, A3, A4, A5, A6] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  def zero: (A1, A2, A3, A4, A5, A6) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero)
}
private[scalaz] trait Tuple7Monoid[A1, A2, A3, A4, A5, A6, A7] extends Monoid[(A1, A2, A3, A4, A5, A6, A7)] with Tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  implicit def _7 : Monoid[A7]
  def zero: (A1, A2, A3, A4, A5, A6, A7) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero, _7.zero)
}
private[scalaz] trait Tuple8Monoid[A1, A2, A3, A4, A5, A6, A7, A8] extends Monoid[(A1, A2, A3, A4, A5, A6, A7, A8)] with Tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8] {
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
  def bind[A, B](fa: Tuple1[A])(f: A => Tuple1[B]) = f(fa._1)
  def point[A](a: => A) = Tuple1(a)
}


// TupleN forms a Monad if the element types other than the last are Monoids.


private[scalaz] trait Tuple2Monad[A1] extends Monad[({type f[x] = (A1, x)})#f] with Tuple2Functor[A1] {
  implicit def _1 : Monoid[A1]
  def bind[A, B](fa: (A1, A))(f: A => (A1, B)) = {
    val t = f(fa._2)

    (_1.append(fa._1, t._1), t._2)
  }
  def point[A](a: => A) = (_1.zero, a)
}
private[scalaz] trait Tuple3Monad[A1, A2] extends Monad[({type f[x] = (A1, A2, x)})#f] with Tuple3Functor[A1, A2] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  def bind[A, B](fa: (A1, A2, A))(f: A => (A1, A2, B)) = {
    val t = f(fa._3)

    (_1.append(fa._1, t._1), _2.append(fa._2, t._2), t._3)
  }

  def point[A](a: => A) = (_1.zero, _2.zero, a)
}
private[scalaz] trait Tuple4Monad[A1, A2, A3] extends Monad[({type f[x] = (A1, A2, A3, x)})#f] with Tuple4Functor[A1, A2, A3] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  def bind[A, B](fa: (A1, A2, A3, A))(f: A => (A1, A2, A3, B)) = {
    val t = f(fa._4)

    (_1.append(fa._1, t._1), _2.append(fa._2, t._2), _3.append(fa._3, t._3), t._4)
  }
  def point[A](a: => A) = (_1.zero, _2.zero, _3.zero, a)
}
private[scalaz] trait Tuple5Monad[A1, A2, A3, A4] extends Monad[({type f[x] = (A1, A2, A3, A4, x)})#f] with Tuple5Functor[A1, A2, A3, A4] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  def bind[A, B](fa: (A1, A2, A3, A4, A))(f: A => (A1, A2, A3, A4, B)) = {
    val t = f(fa._5)

    (_1.append(fa._1, t._1), _2.append(fa._2, t._2), _3.append(fa._3, t._3), _4.append(fa._4, t._4), t._5)
  }
  def point[A](a: => A) = (_1.zero, _2.zero, _3.zero, _4.zero, a)
}
private[scalaz] trait Tuple6Monad[A1, A2, A3, A4, A5] extends Monad[({type f[x] = (A1, A2, A3, A4, A5, x)})#f] with Tuple6Functor[A1, A2, A3, A4, A5] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  def bind[A, B](fa: (A1, A2, A3, A4, A5, A))(f: A => (A1, A2, A3, A4, A5, B)) = {
    val t = f(fa._6)

    (_1.append(fa._1, t._1), _2.append(fa._2, t._2), _3.append(fa._3, t._3), _4.append(fa._4, t._4), _5.append(fa._5, t._5), t._6)
  }
  def point[A](a: => A) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, a)
}
private[scalaz] trait Tuple7Monad[A1, A2, A3, A4, A5, A6] extends Monad[({type f[x] = (A1, A2, A3, A4, A5, A6, x)})#f] with Tuple7Functor[A1, A2, A3, A4, A5, A6] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  def bind[A, B](fa: (A1, A2, A3, A4, A5, A6, A))(f: A => (A1, A2, A3, A4, A5, A6, B)) = {
    val t = f(fa._7)

    (_1.append(fa._1, t._1), _2.append(fa._2, t._2), _3.append(fa._3, t._3), _4.append(fa._4, t._4), _5.append(fa._5, t._5), _6.append(fa._6, t._6), t._7)
  }

  def point[A](a: => A) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero, a)
}
private[scalaz] trait Tuple8Monad[A1, A2, A3, A4, A5, A6, A7] extends Monad[({type f[x] = (A1, A2, A3, A4, A5, A6, A7, x)})#f] with Tuple8Functor[A1, A2, A3, A4, A5, A6, A7] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  implicit def _7 : Monoid[A7]
  def bind[A, B](fa: (A1, A2, A3, A4, A5, A6, A7, A))(f: A => (A1, A2, A3, A4, A5, A6, A7, B)) = {
    val t = f(fa._8)

    (_1.append(fa._1, t._1), _2.append(fa._2, t._2), _3.append(fa._3, t._3), _4.append(fa._4, t._4), _5.append(fa._5, t._5), _6.append(fa._6, t._6), _7.append(fa._7, t._7), t._8)
  }

  def point[A](a: => A) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero, _7.zero, a)
}
