package scalaz

sealed trait Endo[A] {
  def apply(a: A): A
}

object Endo {
  implicit def EndoTo[A](f: A => A) = new Endo[A] {
    def apply(a: A) = f(a)
  }

  implicit def EndoFrom[A](e: Endo[A]) = e.apply(_)
}
