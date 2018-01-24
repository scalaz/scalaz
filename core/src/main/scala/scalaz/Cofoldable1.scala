package scalaz

trait Cofoldable1[F[_], A] {
  
  def unfoldr1[B](b: B)(f: B => (A, Option[B])): F[A]

  def fromNonEmptyList(l: NonEmptyList[A]): F[A] = {
    unfoldr1[NonEmptyList[A]](l)((nel: NonEmptyList[A]) => nel.tail match {
      case ICons(h, t) => (nel.head, Some(NonEmptyList.nel(h, t)))
      case INil() => (nel.head, None)
    })
  }

  trait Cofoldable1Law {
    
    //lwas will prevent instances from creating instances that non parametrically generate As...
    def toNelWeakIdempotence[F[_], A](fa: F[A])(implicit G: Cofoldable1[F, A], E: Equal[NonEmptyList[A]], F: Foldable1[F]): Boolean = {
      val tol: F[A] => NonEmptyList[A] = (fa: F[A]) => F.toNel(G.fromNonEmptyList(F.toNel(fa)))
      E.equal(tol(fa), F.toNel(fa)) || E.equal(tol(fa).reverse, F.toNel(fa)) //we shoulnd't care what direction we add to the list 
    }

    def fromNelWeakIdempotence[F[_], A](fa: F[A])(implicit G: Cofoldable1[F, A], E: Equal[F[A]], F: Foldable1[F]): Boolean = {
      val fl: NonEmptyList[A] => F[A] = (nel: NonEmptyList[A]) => G.fromNonEmptyList(F.toNel(fa))
      E.equal(fl(F.toNel(fa)), G.fromNonEmptyList(F.toNel(fa)))
    }
  }
  
  val cofold1Law = new Cofoldable1Law {}

}

object Cofoldable1 {
  @inline def apply[F[_], A](implicit F: Cofoldable1[F, A]): Cofoldable1[F, A] = F
}

