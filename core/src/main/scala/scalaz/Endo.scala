package scalaz

sealed trait Endo[A] {
  def apply(a: A): A

  def fix: A = apply(fix)
}

trait Endos {
  implicit def EndoTo[A](f: A => A): Endo[A] = new Endo[A] {
    def apply(a: A) = f(a)
  }

  implicit def EndoFrom[A](e: Endo[A]): A => A = e.apply(_)

  def constantEndo[A](a: => A) = EndoTo[A](_ => a)

  def idEndo[A] = EndoTo[A](a => a)
}
