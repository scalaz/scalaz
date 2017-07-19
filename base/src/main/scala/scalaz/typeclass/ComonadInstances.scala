package scalaz
package typeclass

trait ComonadInstances { instances =>
  implicit def tuple2[A1]: Comonad[Tuple2[A1, ?]] = new ComonadClass.Cobind[Tuple2[A1, ?]] {
    override def map[A, B](fa: Tuple2[A1, A])(f: A => B): Tuple2[A1, B] = (fa._1, f(fa._2))
  
    override def cobind[A, B](fa: Tuple2[A1, A])(f: Tuple2[A1, A] => B): Tuple2[A1, B] =
      (fa._1, f(fa))
  
    override def copoint[A](fa: Tuple2[A1, A]): A = fa._2 
  }

  implicit val function0: Comonad[Function0] = new ComonadClass.Cobind[Function0] {
    override def map[A, B](fa: Function0[A])(f: A => B): Function0[B] = () => f(fa())
  
    override def cobind[A, B](fa: Function0[A])(f: Function0[A] => B): Function0[B] =
      () => f(fa)
  
    override def copoint[A](fa: Function0[A]): A = fa() 
  }
}
