package scalaz

sealed trait Endo[A] extends NewType[A => A] {
  def apply(a: A): A = value(a)

  def fix: A = apply(fix)
}

trait Endos {
  def EndoTo[A](f: A => A): Endo[A] = new Endo[A] {
    val value = f
  }

  def constantEndo[A](a: => A): Endo[A] = EndoTo[A](_ => a)

  def idEndo[A]: Endo[A] = EndoTo[A](a => a)
}
