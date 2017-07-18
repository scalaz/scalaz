package scalaz
package typeclass

trait CobindInstances {
  implicit val option: Cobind[Option] = new CobindClass.Cobind[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    
    override def cobind[A, B](fa: Option[A])(f: Option[A] => B): Option[B] =
      Some(f(fa))
  }

  implicit val list: Cobind[List] = new CobindClass.Cobind[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    override def cobind[A, B](fa: List[A])(f: List[A] => B): List[B] =
      List(f(fa))
  }
}
