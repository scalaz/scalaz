package scalaz
package std

sealed trait TupleInstances0 {
  /** Product functor and comonad */
  implicit def tuple2Instance[A1]: Traverse[(A1, ?)] with Comonad[(A1, ?)] = new Tuple2Functor[A1] with Comonad[(A1, ?)] {
    override def cojoin[A](a: (A1, A)) = (a._1, a)
    def copoint[A](p: (A1, A)) = p._2
    def cobind[A, B](fa: (A1, A))(f: ((A1, A)) => B) = (fa._1, f(fa))
  }

  implicit def tuple3Functor[A1, A2]: Traverse[(A1, A2, ?)] = new Tuple3Functor[A1, A2] {}
  implicit def tuple4Functor[A1, A2, A3]: Traverse[(A1, A2, A3, ?)] = new Tuple4Functor[A1, A2, A3] {}
  implicit def tuple5Functor[A1, A2, A3, A4]: Traverse[(A1, A2, A3, A4, ?)] = new Tuple5Functor[A1, A2, A3, A4] {}
  implicit def tuple6Functor[A1, A2, A3, A4, A5]: Traverse[(A1, A2, A3, A4, A5, ?)] = new Tuple6Functor[A1, A2, A3, A4, A5] {}
  implicit def tuple7Functor[A1, A2, A3, A4, A5, A6]: Traverse[(A1, A2, A3, A4, A5, A6, ?)] = new Tuple7Functor[A1, A2, A3, A4, A5, A6] {}
  implicit def tuple8Functor[A1, A2, A3, A4, A5, A6, A7]: Traverse[(A1, A2, A3, A4, A5, A6, A7, ?)] = new Tuple8Functor[A1, A2, A3, A4, A5, A6, A7] {}
}

sealed trait TupleInstances1 extends TupleInstances0 {
  implicit val tuple2Bitraverse: Bitraverse[Tuple2] = new Bitraverse[Tuple2] {
    override def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D) =
      (f(fab._1), g(fab._2))
    def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: (A, B))(f: A => G[C], g: B => G[D]) =
      Applicative[G].tuple2(f(fab._1), g(fab._2))
  }

  implicit val tuple2Associative: Associative[Tuple2] = new Associative[Tuple2] {
    def reassociateLeft[A, B, C](f: (A, (B, C))): ((A, B), C) = ((f._1, f._2._1), f._2._2)
    def reassociateRight[A, B, C](f: ((A, B), C)): (A, (B, C)) = (f._1._1, (f._1._2, f._2))
  }

  implicit def tuple1Semigroup[A1](implicit A1: Semigroup[A1]): Semigroup[Tuple1[A1]] = new Tuple1Semigroup[A1] {
    implicit def _1: Semigroup[A1] = A1
  }
  implicit def tuple2Semigroup[A1, A2](implicit A1: Semigroup[A1], A2: Semigroup[A2]): Semigroup[(A1, A2)] = new Tuple2Semigroup[A1, A2] {
    implicit def _1 = A1
    implicit def _2 = A2
  }
  implicit def tuple3Semigroup[A1, A2, A3](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3]): Semigroup[(A1, A2, A3)] = new Tuple3Semigroup[A1, A2, A3] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
  }
  implicit def tuple4Semigroup[A1, A2, A3, A4](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4]): Semigroup[(A1, A2, A3, A4)] = new Tuple4Semigroup[A1, A2, A3, A4] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
  }
  implicit def tuple5Semigroup[A1, A2, A3, A4, A5](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5]): Semigroup[(A1, A2, A3, A4, A5)] = new Tuple5Semigroup[A1, A2, A3, A4, A5] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
  }
  implicit def tuple6Semigroup[A1, A2, A3, A4, A5, A6](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5], A6: Semigroup[A6]): Semigroup[(A1, A2, A3, A4, A5, A6)] = new Tuple6Semigroup[A1, A2, A3, A4, A5, A6] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
  }
  implicit def tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5], A6: Semigroup[A6], A7: Semigroup[A7]): Semigroup[(A1, A2, A3, A4, A5, A6, A7)] = new Tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7] {
    implicit def _1 = A1
    implicit def _2 = A2
    implicit def _3 = A3
    implicit def _4 = A4
    implicit def _5 = A5
    implicit def _6 = A6
    implicit def _7 = A7
  }
  implicit def tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Semigroup[A1], A2: Semigroup[A2], A3: Semigroup[A3], A4: Semigroup[A4], A5: Semigroup[A5], A6: Semigroup[A6], A7: Semigroup[A7], A8: Semigroup[A8]): Semigroup[(A1, A2, A3, A4, A5, A6, A7, A8)] = new Tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8] {
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
  implicit val tuple1Instance: Traverse[Tuple1] with Monad[Tuple1] with Comonad[Tuple1] = new Tuple1Monad with Tuple1Functor with Comonad[Tuple1] {
    override def cojoin[A](a: Tuple1[A]) = Tuple1(a)
    def copoint[A](p: Tuple1[A]) = p._1
    def cobind[A, B](fa: Tuple1[A])(f: Tuple1[A] => B) = Tuple1(f(fa))
  }

  /** Product BindRec */
  implicit def tuple2BindRec[A1: Semigroup]: BindRec[(A1, ?)] = new Tuple2BindRec[A1] {
    def _1 = implicitly
  }
  implicit def tuple3BindRec[A1: Semigroup, A2: Semigroup]: BindRec[(A1, A2, ?)] = new Tuple3BindRec[A1, A2] {
    def _1 = implicitly
    def _2 = implicitly
  }
  implicit def tuple4BindRec[A1: Semigroup, A2: Semigroup, A3: Semigroup]: BindRec[(A1, A2, A3, ?)] = new Tuple4BindRec[A1, A2, A3] {
    def _1 = implicitly
    def _2 = implicitly
    def _3 = implicitly
  }
  implicit def tuple5BindRec[A1: Semigroup, A2: Semigroup, A3: Semigroup, A4: Semigroup]: BindRec[(A1, A2, A3, A4, ?)] = new Tuple5BindRec[A1, A2, A3, A4] {
    def _1 = implicitly
    def _2 = implicitly
    def _3 = implicitly
    def _4 = implicitly
  }
  implicit def tuple6BindRec[A1: Semigroup, A2: Semigroup, A3: Semigroup, A4: Semigroup, A5: Semigroup]: BindRec[(A1, A2, A3, A4, A5, ?)] = new Tuple6BindRec[A1, A2, A3, A4, A5] {
    def _1 = implicitly
    def _2 = implicitly
    def _3 = implicitly
    def _4 = implicitly
    def _5 = implicitly
  }
  implicit def tuple7BindRec[A1: Semigroup, A2: Semigroup, A3: Semigroup, A4: Semigroup, A5: Semigroup, A6: Semigroup]: BindRec[(A1, A2, A3, A4, A5, A6, ?)] = new Tuple7BindRec[A1, A2, A3, A4, A5, A6] {
    def _1 = implicitly
    def _2 = implicitly
    def _3 = implicitly
    def _4 = implicitly
    def _5 = implicitly
    def _6 = implicitly
  }
  implicit def tuple8BindRec[A1: Semigroup, A2: Semigroup, A3: Semigroup, A4: Semigroup, A5: Semigroup, A6: Semigroup, A7: Semigroup]: BindRec[(A1, A2, A3, A4, A5, A6, A7, ?)] = new Tuple8BindRec[A1, A2, A3, A4, A5, A6, A7] {
    def _1 = implicitly
    def _2 = implicitly
    def _3 = implicitly
    def _4 = implicitly
    def _5 = implicitly
    def _6 = implicitly
    def _7 = implicitly
  }

  implicit def tuple1Equal[A1](implicit A1: Equal[A1]): Equal[Tuple1[A1]] =
    new Tuple1Equal[A1] {
      implicit def _1 = A1
    }
  implicit def tuple2Equal[A1, A2](implicit A1: Equal[A1], A2: Equal[A2]): Equal[(A1, A2)] = 
    new Tuple2Equal[A1, A2] {
      implicit def _1 = A1
      implicit def _2 = A2
    }
  implicit def tuple3Equal[A1, A2, A3](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3]): Equal[(A1, A2, A3)] = 
    new Tuple3Equal[A1, A2, A3] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
    }
  implicit def tuple4Equal[A1, A2, A3, A4](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4]): Equal[(A1, A2, A3, A4)] = 
    new Tuple4Equal[A1, A2, A3, A4] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
    }
  implicit def tuple5Equal[A1, A2, A3, A4, A5](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5]): Equal[(A1, A2, A3, A4, A5)] = 
    new Tuple5Equal[A1, A2, A3, A4, A5] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
    }
  implicit def tuple6Equal[A1, A2, A3, A4, A5, A6](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5], A6: Equal[A6]): Equal[(A1, A2, A3, A4, A5, A6)] =
    new Tuple6Equal[A1, A2, A3, A4, A5, A6] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
    }
  implicit def tuple7Equal[A1, A2, A3, A4, A5, A6, A7](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5], A6: Equal[A6], A7: Equal[A7]): Equal[(A1, A2, A3, A4, A5, A6, A7)] =
    new Tuple7Equal[A1, A2, A3, A4, A5, A6, A7] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
      implicit def _7 = A7
    }
  implicit def tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Equal[A1], A2: Equal[A2], A3: Equal[A3], A4: Equal[A4], A5: Equal[A5], A6: Equal[A6], A7: Equal[A7], A8: Equal[A8]): Equal[(A1, A2, A3, A4, A5, A6, A7, A8)] =
    new Tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8] {
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
sealed trait TupleInstances2 extends TupleInstances1 {

  implicit def tuple1Show[A1](implicit A1: Show[A1]): Show[Tuple1[A1]] = 
    new Tuple1Show[A1] {
      implicit def _1 = A1
    }
  implicit def tuple2Show[A1, A2](implicit A1: Show[A1], A2: Show[A2]): Show[(A1, A2)] =
    new Tuple2Show[A1, A2] {
      implicit def _1 = A1
      implicit def _2 = A2
    }
  implicit def tuple3Show[A1, A2, A3](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3]): Show[(A1, A2, A3)] =
    new Tuple3Show[A1, A2, A3] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
    }
  implicit def tuple4Show[A1, A2, A3, A4](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4]): Show[(A1, A2, A3, A4)] =
    new Tuple4Show[A1, A2, A3, A4] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
    }
  implicit def tuple5Show[A1, A2, A3, A4, A5](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5]): Show[(A1, A2, A3, A4, A5)] =
    new Tuple5Show[A1, A2, A3, A4, A5] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
    }
  implicit def tuple6Show[A1, A2, A3, A4, A5, A6](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5], A6: Show[A6]): Show[(A1, A2, A3, A4, A5, A6)] =
    new Tuple6Show[A1, A2, A3, A4, A5, A6] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
    }
  implicit def tuple7Show[A1, A2, A3, A4, A5, A6, A7](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5], A6: Show[A6], A7: Show[A7]): Show[(A1, A2, A3, A4, A5, A6, A7)] =
    new Tuple7Show[A1, A2, A3, A4, A5, A6, A7] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
      implicit def _7 = A7
    }
  implicit def tuple8Show[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Show[A1], A2: Show[A2], A3: Show[A3], A4: Show[A4], A5: Show[A5], A6: Show[A6], A7: Show[A7], A8: Show[A8]): Show[(A1, A2, A3, A4, A5, A6, A7, A8)] =
    new Tuple8Show[A1, A2, A3, A4, A5, A6, A7, A8] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
      implicit def _7 = A7
      implicit def _8 = A8
    }

  implicit def tuple1Order[A1](implicit A1: Order[A1]): Order[Tuple1[A1]] =
    new Tuple1Order[A1] {
      implicit def _1 = A1
    }
  implicit def tuple2Order[A1, A2](implicit A1: Order[A1], A2: Order[A2]): Order[(A1, A2)] =
    new Tuple2Order[A1, A2] {
      implicit def _1 = A1
      implicit def _2 = A2
    }
  implicit def tuple3Order[A1, A2, A3](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3]): Order[(A1, A2, A3)] =
    new Tuple3Order[A1, A2, A3] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
    }
  implicit def tuple4Order[A1, A2, A3, A4](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4]): Order[(A1, A2, A3, A4)] =
    new Tuple4Order[A1, A2, A3, A4] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
    }
  implicit def tuple5Order[A1, A2, A3, A4, A5](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5]): Order[(A1, A2, A3, A4, A5)] =
    new Tuple5Order[A1, A2, A3, A4, A5] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
    }
  implicit def tuple6Order[A1, A2, A3, A4, A5, A6](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6]): Order[(A1, A2, A3, A4, A5, A6)] = 
    new Tuple6Order[A1, A2, A3, A4, A5, A6] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
    }
  implicit def tuple7Order[A1, A2, A3, A4, A5, A6, A7](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7]): Order[(A1, A2, A3, A4, A5, A6, A7)] =
    new Tuple7Order[A1, A2, A3, A4, A5, A6, A7] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
      implicit def _7 = A7
    }
  implicit def tuple8Order[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8]): Order[(A1, A2, A3, A4, A5, A6, A7, A8)] =
    new Tuple8Order[A1, A2, A3, A4, A5, A6, A7, A8] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
      implicit def _7 = A7
      implicit def _8 = A8
    }
  implicit def tuple1Monoid[A1](implicit A1: Monoid[A1]): Monoid[Tuple1[A1]] =
    new Tuple1Monoid[A1] {
      implicit def _1 = A1
    }
  implicit def tuple2Monoid[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]): Monoid[(A1, A2)] =
    new Tuple2Monoid[A1, A2] {
      implicit def _1 = A1
      implicit def _2 = A2
    }
  implicit def tuple3Monoid[A1, A2, A3](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3]): Monoid[(A1, A2, A3)] =
    new Tuple3Monoid[A1, A2, A3] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
    }
  implicit def tuple4Monoid[A1, A2, A3, A4](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4]): Monoid[(A1, A2, A3, A4)] =
    new Tuple4Monoid[A1, A2, A3, A4] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
    }
  implicit def tuple5Monoid[A1, A2, A3, A4, A5](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5]): Monoid[(A1, A2, A3, A4, A5)] =
    new Tuple5Monoid[A1, A2, A3, A4, A5] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
    }
  implicit def tuple6Monoid[A1, A2, A3, A4, A5, A6](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6]): Monoid[(A1, A2, A3, A4, A5, A6)] =
    new Tuple6Monoid[A1, A2, A3, A4, A5, A6] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
    }
  implicit def tuple7Monoid[A1, A2, A3, A4, A5, A6, A7](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6], A7: Monoid[A7]): Monoid[(A1, A2, A3, A4, A5, A6, A7)] =
    new Tuple7Monoid[A1, A2, A3, A4, A5, A6, A7] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
      implicit def _7 = A7
    }
  implicit def tuple8Monoid[A1, A2, A3, A4, A5, A6, A7, A8](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6], A7: Monoid[A7], A8: Monoid[A8]): Monoid[(A1, A2, A3, A4, A5, A6, A7, A8)] =
    new Tuple8Monoid[A1, A2, A3, A4, A5, A6, A7, A8] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
      implicit def _7 = A7
      implicit def _8 = A8
    }

  implicit val tuple1Cozip: Cozip[Tuple1] = new Tuple1Cozip {}
  implicit def tuple2Cozip[A1]: Cozip[(A1, ?)] = new Tuple2Cozip[A1] {}
  implicit def tuple3Cozip[A1, A2]: Cozip[(A1, A2, ?)] = new Tuple3Cozip[A1, A2] {}
  implicit def tuple4Cozip[A1, A2, A3]: Cozip[(A1, A2, A3, ?)] = new Tuple4Cozip[A1, A2, A3] {}
  implicit def tuple5Cozip[A1, A2, A3, A4]: Cozip[(A1, A2, A3, A4, ?)] = new Tuple5Cozip[A1, A2, A3, A4] {}
  implicit def tuple6Cozip[A1, A2, A3, A4, A5]: Cozip[(A1, A2, A3, A4, A5, ?)] = new Tuple6Cozip[A1, A2, A3, A4, A5] {}
  implicit def tuple7Cozip[A1, A2, A3, A4, A5, A6]: Cozip[(A1, A2, A3, A4, A5, A6, ?)] = new Tuple7Cozip[A1, A2, A3, A4, A5, A6] {}
  implicit def tuple8Cozip[A1, A2, A3, A4, A5, A6, A7]: Cozip[(A1, A2, A3, A4, A5, A6, A7, ?)] = new Tuple8Cozip[A1, A2, A3, A4, A5, A6, A7] {}

  implicit def tuple2Monad[A1](implicit A1: Monoid[A1]): Monad[(A1, ?)] =
    new Tuple2Monad[A1] {
      implicit def _1 = A1
    }
  implicit def tuple3Monad[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]): Monad[(A1, A2, ?)] =
    new Tuple3Monad[A1, A2] {
      implicit def _1 = A1
      implicit def _2 = A2
    }
  implicit def tuple4Monad[A1, A2, A3](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3]): Monad[(A1, A2, A3, ?)] =
    new Tuple4Monad[A1, A2, A3] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
    }
  implicit def tuple5Monad[A1, A2, A3, A4](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4]): Monad[(A1, A2, A3, A4, ?)] =
    new Tuple5Monad[A1, A2, A3, A4] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
    }
  implicit def tuple6Monad[A1, A2, A3, A4, A5](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5]): Monad[(A1, A2, A3, A4, A5, ?)] =
    new Tuple6Monad[A1, A2, A3, A4, A5] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
    }
  implicit def tuple7Monad[A1, A2, A3, A4, A5, A6](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6]): Monad[(A1, A2, A3, A4, A5, A6, ?)] =
    new Tuple7Monad[A1, A2, A3, A4, A5, A6] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
    }
  implicit def tuple8Monad[A1, A2, A3, A4, A5, A6, A7](implicit A1: Monoid[A1], A2: Monoid[A2], A3: Monoid[A3], A4: Monoid[A4], A5: Monoid[A5], A6: Monoid[A6], A7: Monoid[A7]): Monad[(A1, A2, A3, A4, A5, A6, A7, ?)] =
    new Tuple8Monad[A1, A2, A3, A4, A5, A6, A7] {
      implicit def _1 = A1
      implicit def _2 = A2
      implicit def _3 = A3
      implicit def _4 = A4
      implicit def _5 = A5
      implicit def _6 = A6
      implicit def _7 = A7
    }

}

trait TupleInstances extends TupleInstances2

object tuple extends TupleInstances {
  object tupleSyntax extends scalaz.syntax.std.ToTupleOps
}

private trait Tuple1Semigroup[A1] extends Semigroup[Tuple1[A1]] {
  implicit def _1 : Semigroup[A1]
  def append(f1: Tuple1[A1], f2: => Tuple1[A1]) = (
    Tuple1(Semigroup[A1].append(f1._1, f2._1))
    )
}

private trait Tuple2Semigroup[A1, A2] extends Semigroup[(A1, A2)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  def append(f1: (A1, A2), _f2: => (A1, A2)) = {
    val f2 = Need(_f2)
    (
    _1.append(f1._1, f2.value._1),
    _2.append(f1._2, f2.value._2)
    )
  }
}
private trait Tuple3Semigroup[A1, A2, A3] extends Semigroup[(A1, A2, A3)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  def append(f1: (A1, A2, A3), _f2: => (A1, A2, A3)) = {
    val f2 = Need(_f2)
    (
    _1.append(f1._1, f2.value._1),
    _2.append(f1._2, f2.value._2),
    _3.append(f1._3, f2.value._3)
    )
  }
}
private trait Tuple4Semigroup[A1, A2, A3, A4] extends Semigroup[(A1, A2, A3, A4)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  def append(f1: (A1, A2, A3, A4), _f2: => (A1, A2, A3, A4)) = {
    val f2 = Need(_f2)
    (
    _1.append(f1._1, f2.value._1),
    _2.append(f1._2, f2.value._2),
    _3.append(f1._3, f2.value._3),
    _4.append(f1._4, f2.value._4)
    )
  }
}
private trait Tuple5Semigroup[A1, A2, A3, A4, A5] extends Semigroup[(A1, A2, A3, A4, A5)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  implicit def _5 : Semigroup[A5]
  def append(f1: (A1, A2, A3, A4, A5), _f2: => (A1, A2, A3, A4, A5)) = {
    val f2 = Need(_f2)
    (
    _1.append(f1._1, f2.value._1),
    _2.append(f1._2, f2.value._2),
    _3.append(f1._3, f2.value._3),
    _4.append(f1._4, f2.value._4),
    _5.append(f1._5, f2.value._5)
    )
  }
}
private trait Tuple6Semigroup[A1, A2, A3, A4, A5, A6] extends Semigroup[(A1, A2, A3, A4, A5, A6)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  implicit def _5 : Semigroup[A5]
  implicit def _6 : Semigroup[A6]
  def append(f1: (A1, A2, A3, A4, A5, A6), _f2: => (A1, A2, A3, A4, A5, A6)) = {
    val f2 = Need(_f2)
    (
    _1.append(f1._1, f2.value._1),
    _2.append(f1._2, f2.value._2),
    _3.append(f1._3, f2.value._3),
    _4.append(f1._4, f2.value._4),
    _5.append(f1._5, f2.value._5),
    _6.append(f1._6, f2.value._6)
    )
  }
}
private trait Tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7] extends Semigroup[(A1, A2, A3, A4, A5, A6, A7)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  implicit def _5 : Semigroup[A5]
  implicit def _6 : Semigroup[A6]
  implicit def _7 : Semigroup[A7]
  def append(f1: (A1, A2, A3, A4, A5, A6, A7), _f2: => (A1, A2, A3, A4, A5, A6, A7)) = {
    val f2 = Need(_f2)
    (
    _1.append(f1._1, f2.value._1),
    _2.append(f1._2, f2.value._2),
    _3.append(f1._3, f2.value._3),
    _4.append(f1._4, f2.value._4),
    _5.append(f1._5, f2.value._5),
    _6.append(f1._6, f2.value._6),
    _7.append(f1._7, f2.value._7)
    )
  }
}
private trait Tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8] extends Semigroup[(A1, A2, A3, A4, A5, A6, A7, A8)] {
  implicit def _1 : Semigroup[A1]
  implicit def _2 : Semigroup[A2]
  implicit def _3 : Semigroup[A3]
  implicit def _4 : Semigroup[A4]
  implicit def _5 : Semigroup[A5]
  implicit def _6 : Semigroup[A6]
  implicit def _7 : Semigroup[A7]
  implicit def _8 : Semigroup[A8]
  def append(f1: (A1, A2, A3, A4, A5, A6, A7, A8), _f2: => (A1, A2, A3, A4, A5, A6, A7, A8)) = {
    val f2 = Need(_f2)
    (
    _1.append(f1._1, f2.value._1),
    _2.append(f1._2, f2.value._2),
    _3.append(f1._3, f2.value._3),
    _4.append(f1._4, f2.value._4),
    _5.append(f1._5, f2.value._5),
    _6.append(f1._6, f2.value._6),
    _7.append(f1._7, f2.value._7),
    _8.append(f1._8, f2.value._8)
    )
  }
}
private trait Tuple1Functor extends Traverse[Tuple1] {
  override def map[A, B](fa: Tuple1[A])(f: A => B) =
    Tuple1(f(fa._1))
  def traverseImpl[G[_], A, B](fa: Tuple1[A])(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(f(fa._1))(Tuple1.apply)
}

private trait Tuple1Cozip extends Cozip[Tuple1] {
  override def cozip[A, B](x: Tuple1[A \/ B]) =
    x._1.bimap(Tuple1(_), Tuple1(_))
}
private trait Tuple2Cozip[A1] extends Cozip[(A1, ?)] {
  override def cozip[A, B](x: (A1, A \/ B)) =
    x._2.bimap((x._1, _), (x._1, _))
}
private trait Tuple3Cozip[A1, A2] extends Cozip[(A1, A2, ?)] {
  override def cozip[A, B](x: (A1, A2, A \/ B)) =
    x._3.bimap((x._1, x._2, _), (x._1, x._2, _))
}
private trait Tuple4Cozip[A1, A2, A3] extends Cozip[(A1, A2, A3, ?)] {
  override def cozip[A, B](x: (A1, A2, A3, A \/ B)) =
    x._4.bimap((x._1, x._2, x._3, _), (x._1, x._2, x._3, _))
}
private trait Tuple5Cozip[A1, A2, A3, A4] extends Cozip[(A1, A2, A3, A4, ?)] {
  override def cozip[A, B](x: (A1, A2, A3, A4, A \/ B)) =
    x._5.bimap((x._1, x._2, x._3, x._4, _), (x._1, x._2, x._3, x._4, _))
}
private trait Tuple6Cozip[A1, A2, A3, A4, A5] extends Cozip[(A1, A2, A3, A4, A5, ?)] {
  override def cozip[A, B](x: (A1, A2, A3, A4, A5, A \/ B)) =
    x._6.bimap((x._1, x._2, x._3, x._4, x._5, _), (x._1, x._2, x._3, x._4, x._5, _))
}
private trait Tuple7Cozip[A1, A2, A3, A4, A5, A6] extends Cozip[(A1, A2, A3, A4, A5, A6, ?)] {
  override def cozip[A, B](x: (A1, A2, A3, A4, A5, A6, A \/ B)) =
    x._7.bimap((x._1, x._2, x._3, x._4, x._5, x._6, _), (x._1, x._2, x._3, x._4, x._5, x._6, _))
}
private trait Tuple8Cozip[A1, A2, A3, A4, A5, A6, A7] extends Cozip[(A1, A2, A3, A4, A5, A6, A7, ?)] {
  override def cozip[A, B](x: (A1, A2, A3, A4, A5, A6, A7, A \/ B)) =
    x._8.bimap((x._1, x._2, x._3, x._4, x._5, x._6, x._7, _), (x._1, x._2, x._3, x._4, x._5, x._6, x._7, _))
}

private trait Tuple1Equal[A1] extends Equal[Tuple1[A1]] {
  implicit def _1 : Equal[A1]
  override def equal(f1: Tuple1[A1], f2: Tuple1[A1]) = _1.equal(f1._1, f2._1)
  override val equalIsNatural: Boolean = _1.equalIsNatural
}
private trait Tuple2Equal[A1, A2] extends Equal[(A1, A2)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  override def equal(f1: (A1, A2), f2: (A1, A2)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural
}
private trait Tuple3Equal[A1, A2, A3] extends Equal[(A1, A2, A3)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  override def equal(f1: (A1, A2, A3), f2: (A1, A2, A3)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural && _3.equalIsNatural
}
private trait Tuple4Equal[A1, A2, A3, A4] extends Equal[(A1, A2, A3, A4)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  override def equal(f1: (A1, A2, A3, A4), f2: (A1, A2, A3, A4)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural && _3.equalIsNatural && _4.equalIsNatural
}
private trait Tuple5Equal[A1, A2, A3, A4, A5] extends Equal[(A1, A2, A3, A4, A5)] {
  implicit def _1 : Equal[A1]
  implicit def _2 : Equal[A2]
  implicit def _3 : Equal[A3]
  implicit def _4 : Equal[A4]
  implicit def _5 : Equal[A5]
  override def equal(f1: (A1, A2, A3, A4, A5), f2: (A1, A2, A3, A4, A5)) =
    _1.equal(f1._1, f2._1) && _2.equal(f1._2, f2._2) && _3.equal(f1._3, f2._3) && _4.equal(f1._4, f2._4) && _5.equal(f1._5, f2._5)
  override val equalIsNatural: Boolean = _1.equalIsNatural && _2.equalIsNatural && _3.equalIsNatural && _4.equalIsNatural && _5.equalIsNatural
}
private trait Tuple6Equal[A1, A2, A3, A4, A5, A6] extends Equal[(A1, A2, A3, A4, A5, A6)] {
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
private trait Tuple7Equal[A1, A2, A3, A4, A5, A6, A7] extends Equal[(A1, A2, A3, A4, A5, A6, A7)] {
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
private trait Tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8] extends Equal[(A1, A2, A3, A4, A5, A6, A7, A8)] {
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
private trait Tuple1Show[A1] extends Show[Tuple1[A1]] {
  implicit def _1 : Show[A1]
  override def show(f: Tuple1[A1]) =
    Cord("(", _1.show(f._1), ")")
}
private trait Tuple2Show[A1, A2] extends Show[(A1, A2)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  override def show(f: (A1, A2)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ")")
}
private trait Tuple3Show[A1, A2, A3] extends Show[(A1, A2, A3)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  override def show(f: (A1, A2, A3)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ")")
}
private trait Tuple4Show[A1, A2, A3, A4] extends Show[(A1, A2, A3, A4)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  override def show(f: (A1, A2, A3, A4)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ",", _4.show(f._4), ")")
}
private trait Tuple5Show[A1, A2, A3, A4, A5] extends Show[(A1, A2, A3, A4, A5)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  implicit def _5 : Show[A5]
  override def show(f: (A1, A2, A3, A4, A5)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ",", _4.show(f._4), ",", _5.show(f._5), ")")
}
private trait Tuple6Show[A1, A2, A3, A4, A5, A6] extends Show[(A1, A2, A3, A4, A5, A6)] {
  implicit def _1 : Show[A1]
  implicit def _2 : Show[A2]
  implicit def _3 : Show[A3]
  implicit def _4 : Show[A4]
  implicit def _5 : Show[A5]
  implicit def _6 : Show[A6]
  override def show(f: (A1, A2, A3, A4, A5, A6)) =
    Cord("(", _1.show(f._1), ",", _2.show(f._2), ",", _3.show(f._3), ",", _4.show(f._4), ",", _5.show(f._5), ",", _6.show(f._6), ")")
}
private trait Tuple7Show[A1, A2, A3, A4, A5, A6, A7] extends Show[(A1, A2, A3, A4, A5, A6, A7)] {
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
private trait Tuple8Show[A1, A2, A3, A4, A5, A6, A7, A8] extends Show[(A1, A2, A3, A4, A5, A6, A7, A8)] {
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

private trait Tuple1Order[A1] extends Order[Tuple1[A1]] with Tuple1Equal[A1] {
  implicit def _1 : Order[A1]
  def order(f1: Tuple1[A1], f2: Tuple1[A1]) = _1.order(f1._1, f2._1)
}
private trait Tuple2Order[A1, A2] extends Order[(A1, A2)] with Tuple2Equal[A1, A2] {
  implicit def _1 : Order[A1]
  implicit def _2 : Order[A2]
  import Ordering.EQ
  def order(f1: (A1, A2), f2: (A1, A2)) =
    (_1.order(f1._1, f2._1), _2.order(f1._2, f2._2)) match {
      case (EQ, ord) => ord
      case (ord, _) => ord
    }
}
private trait Tuple3Order[A1, A2, A3] extends Order[(A1, A2, A3)] with Tuple3Equal[A1, A2, A3]{
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
private trait Tuple4Order[A1, A2, A3, A4] extends Order[(A1, A2, A3, A4)] with Tuple4Equal[A1, A2, A3, A4]{
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
private trait Tuple5Order[A1, A2, A3, A4, A5] extends Order[(A1, A2, A3, A4, A5)] with Tuple5Equal[A1, A2, A3, A4, A5] {
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
private trait Tuple6Order[A1, A2, A3, A4, A5, A6] extends Order[(A1, A2, A3, A4, A5, A6)] with Tuple6Equal[A1, A2, A3, A4, A5, A6] {
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
private trait Tuple7Order[A1, A2, A3, A4, A5, A6, A7] extends Order[(A1, A2, A3, A4, A5, A6, A7)] with Tuple7Equal[A1, A2, A3, A4, A5, A6, A7]{
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
private trait Tuple8Order[A1, A2, A3, A4, A5, A6, A7, A8] extends Order[(A1, A2, A3, A4, A5, A6, A7, A8)] with Tuple8Equal[A1, A2, A3, A4, A5, A6, A7, A8] {
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

private trait Tuple1Monoid[A1] extends Monoid[Tuple1[A1]] with Tuple1Semigroup[A1] {
  implicit def _1 : Monoid[A1]
  def zero: Tuple1[A1] = Tuple1(_1.zero)
}
private trait Tuple2Monoid[A1, A2] extends Monoid[(A1, A2)] with Tuple2Semigroup[A1, A2] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  def zero: (A1, A2) = (_1.zero, _2.zero)
}
private trait Tuple3Monoid[A1, A2, A3] extends Monoid[(A1, A2, A3)] with Tuple3Semigroup[A1, A2, A3] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  def zero: (A1, A2, A3) = (_1.zero, _2.zero, _3.zero)
}
private trait Tuple4Monoid[A1, A2, A3, A4] extends Monoid[(A1, A2, A3, A4)] with Tuple4Semigroup[A1, A2, A3, A4] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  def zero: (A1, A2, A3, A4) = (_1.zero, _2.zero, _3.zero, _4.zero)
}
private trait Tuple5Monoid[A1, A2, A3, A4, A5] extends Monoid[(A1, A2, A3, A4, A5)] with Tuple5Semigroup[A1, A2, A3, A4, A5] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  def zero: (A1, A2, A3, A4, A5) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero)
}
private trait Tuple6Monoid[A1, A2, A3, A4, A5, A6] extends Monoid[(A1, A2, A3, A4, A5, A6)] with Tuple6Semigroup[A1, A2, A3, A4, A5, A6] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  def zero: (A1, A2, A3, A4, A5, A6) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero)
}
private trait Tuple7Monoid[A1, A2, A3, A4, A5, A6, A7] extends Monoid[(A1, A2, A3, A4, A5, A6, A7)] with Tuple7Semigroup[A1, A2, A3, A4, A5, A6, A7] {
  implicit def _1 : Monoid[A1]
  implicit def _2 : Monoid[A2]
  implicit def _3 : Monoid[A3]
  implicit def _4 : Monoid[A4]
  implicit def _5 : Monoid[A5]
  implicit def _6 : Monoid[A6]
  implicit def _7 : Monoid[A7]
  def zero: (A1, A2, A3, A4, A5, A6, A7) = (_1.zero, _2.zero, _3.zero, _4.zero, _5.zero, _6.zero, _7.zero)
}
private trait Tuple8Monoid[A1, A2, A3, A4, A5, A6, A7, A8] extends Monoid[(A1, A2, A3, A4, A5, A6, A7, A8)] with Tuple8Semigroup[A1, A2, A3, A4, A5, A6, A7, A8] {
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

private trait Tuple1Monad extends Monad[Tuple1] {
  def bind[A, B](fa: Tuple1[A])(f: A => Tuple1[B]) = f(fa._1)
  def point[A](a: => A) = Tuple1(a)
}
