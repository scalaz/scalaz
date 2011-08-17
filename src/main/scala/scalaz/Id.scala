package scalaz

trait Ids { 
  type Id[X] = X
  val id = new MonadInstance[Id] {
    def pure[A](a: => A): A = a
    def bind[A,B](a: A)(f: A => B): B = f(a)
  }
}

object Id extends Ids
