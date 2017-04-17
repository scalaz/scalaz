package scalaz
package typeclass

trait ProfunctorInstances { instances =>
  implicit val function: Profunctor[Function] = new ProfunctorClass.Dimap[Function] {
 
    override def dimap[A, B, C, D](fab: Function[A, B])(ca: C => A)(bd: B => D): Function[C, D] =
      (c: C) => bd(fab(ca(c)))
   
    override def lmap[A, B, C](fab: Function[A, B])(ca: C => A): Function[C, B] =
      (c: C) => fab(ca(c))
    
    override def rmap[A, B, C](fab: Function[A, B])(bc: B => C): Function[A, C] =
      fab andThen bc
   } 
}
