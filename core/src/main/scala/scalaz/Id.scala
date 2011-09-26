package scalaz

trait Ids { 
  val id = new Monad[Id] {
    def pure[A](a: => A): A = a
    def bind[A,B](a: A)(f: A => B): B = f(a)
  }
}

object Id extends Ids
