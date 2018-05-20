package scalaz
package typeclass

trait BifunctorInstances {

  implicit final val tuple2Bifunctor: Bifunctor[Tuple2] =
    instanceOf(new BifunctorClass[Tuple2] {
      val minimal = meta.IsMinimal()
      override def lmap[A, B, S](fab: (A, B))(f: A => S): (S, B) =
        fab.copy(_1 = f(fab._1))
      override def rmap[A, B, T](fab: (A, B))(f: B => T): (A, T) =
        fab.copy(_2 = f(fab._2))
    })
}
