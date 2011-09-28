package scalaz

trait Idents {
  val id = new Monad[Id] {
    def pure[A](a: => A): A = a
    def bind[A,B](a: A)(f: A => B): B = f(a)
  }
}

// Not named Id so as not to trigger https://issues.scala-lang.org/browse/SI-5031
object Ident extends Idents
