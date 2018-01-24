package scalaz

trait Cofoldable[F[_], A] extends Cofoldable1[F, A] {
  
  def unfoldr[B](b: B)(f: B => Option[(B, A)]): F[A]
  
  def unfoldr1[B](b: B)(f: B => (A, Option[B])): F[A] =
    unfoldr(b)(b => f(b) match {
      case (a, Some(b)) => Some((b, a))
      case (a, None) => None
    })
  
  //DERIVED 
 
  def fromIList(l: IList[A]): F[A] = 
    unfoldr[IList[A]](l)(_ match {
      case ICons(h, t) => Some((t, h))
      case INil() => None
    })
  
  def filterAll(fa: F[A], f: A => Boolean)(implicit F: Foldable[F]): F[A] = 
    fromIList(F.toIList(fa).filter(f))

  def collectall[B](fa: F[A], pf: PartialFunction[A, A])(implicit F: Foldable[F]): F[A] = 
    fromIList(F.toIList(fa).collect(pf))

  def splitup(fa: F[A], f: A => Boolean)(implicit F: Foldable[F]): (F[A], F[A]) = {
    val t = F.toIList(fa).partition(f)
    (fromIList(t._1), fromIList(t._2))
  }

   
  trait CofoldLaw {
    
    //lwas will prevent instances from creating instances that non parametrically generate As...

    def toListWeakIdempotence[F[_], A](fa: F[A])(implicit G: Cofoldable[F, A], E: Equal[IList[A]], F: Foldable[F]): Boolean = {
      val tol: F[A] => IList[A] = (fa: F[A]) => F.toIList(G.fromIList(F.toIList(fa)))
      E.equal(tol(fa), F.toIList(fa)) || E.equal(tol(fa).reverse, F.toIList(fa)) //we shoulnd't care what direction we add to the list 
    }

    def fromListWeakIdempotence[F[_], A](fa: F[A])(implicit G: Cofoldable[F, A], E: Equal[F[A]], F: Foldable[F]): Boolean = {
      val fl: IList[A] => F[A] = (il: IList[A]) => G.fromIList(F.toIList(G.fromIList(il)))
      E.equal(fl(F.toIList(fa).reverse), G.fromIList(F.toIList(fa))) 
    }
  }
  
  val cofoldLaw = new CofoldLaw {}
}

object Cofoldable {
  @inline def apply[F[_], A](implicit F: Cofoldable[F, A]): Cofoldable[F, A] = F
}


