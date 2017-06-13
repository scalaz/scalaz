package scalaz
package typeclass

trait StrongInstances { instances =>
  implicit val function: Strong[Function] = new StrongClass[Function] {

    override def dimap[A, B, C, D](fab: Function[A, B])(ca: C => A)(bd: B => D): Function[C, D] =
      profunctor.dimap(fab)(ca)(bd)
   
    override def lmap[A, B, C](fab: Function[A, B])(ca: C => A): Function[C, B] =
      profunctor.lmap(fab)(ca)
    
    override def rmap[A, B, C](fab: Function[A, B])(bc: B => C): Function[A, C] =
      profunctor.rmap(fab)(bc)
    
    override def first[A, B, C](pab: A => B): ((A, C)) => (B, C) = _ match {
      case (a, c) => (pab(a), c)
    }

    override def second[A, B, C](pab: A => B): ((C, A)) => (C, B) = _ match {
      case (c, a) => (c, pab(a))
    }
  }
}
