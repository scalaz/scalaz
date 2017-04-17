package scalaz
package data 

object ProCompose {
  import typeclass.{Profunctor, Strong, Category, StrongClass, CategoryClass}
  import typeclass.Strong._
  import typeclass.Profunctor._

  trait ProCompose[P[_, _], A, B] { } 

  case class Pure[P[_, _], A, B](pab: A => B) extends ProCompose[P, A, B]

  case class Composed[P[_, _], A, X, B](pab: P[A, X], more: ProCompose[P, X, B]) extends ProCompose[P, A, B]

  implicit def ProComposeStrong[P[_, _]: Strong]: StrongClass.First[ProCompose[P, ?, ?]] = new StrongClass.First[ProCompose[P, ?, ?]] with CategoryClass[ProCompose[P, ?, ?]] { 
    override def dimap[A, B, C, D](fab: ProCompose[P, A, B])(fca: C => A)(fbd: B => D): ProCompose[P, C, D] = 
      fab match {
        case Composed(pab, more) => Composed(pab.lmap(fca), rmap(more)(fbd))
        case Pure(f) => Pure(f.dimap(fca)(fbd))
      }   

    override def rmap[A, B, D](fab: ProCompose[P, A, B])(fbd: B => D): ProCompose[P, A, D] =
      fab match {
        case Composed(pab, more) => Composed(pab, rmap(more)(fbd))
        case Pure(f) => Pure(f.rmap(fbd))
      }  

    override def lmap[A, B, C](fab: ProCompose[P, A, B])(fca: C => A): ProCompose[P, C, B] =
      fab match {
        case Composed(pab, more) => Composed(pab.lmap(fca), more)
        case Pure(f) => Pure(f.lmap(fca))
      }   
  
    override def first[A, B, C](fab: ProCompose[P,A,B]): ProCompose[P,(A, C),(B, C)] = 
      fab match {
        case Composed(pab, more) => Composed(Strong[P].first(pab), first(more))
        case Pure(f) => Pure(Strong[Function].first(f))
      }   

    override def second[A, B, C](fab: ProCompose[P,A,B]): ProCompose[P,(C, A),(C, B)] = 
      fab match {
        case Composed(pab, more) => Composed(Strong[P].second(pab), second(more))
        case Pure(f) => Pure(Strong[Function].second(f))
      }

    override def id[A]: ProCompose[P,A,A] = Pure((a: A) => a)
    
    override def compose[A, B, C](fbc: ProCompose[P, B, C], fab: ProCompose[P, A, B]): ProCompose[P, A, C] = fab match {
        case Composed(pab, more) =>  
          Composed(pab, compose(fbc, more)) 
        case Pure(f) => lmap(fbc)(f) 
      }
  }
}
