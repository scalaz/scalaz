package scalaz

sealed trait Endo[A] {
  def apply(a: => A): A

  import Endo._

  def fix: A = apply(fix)

  def compose(e: Endo[A]): Endo[A] =
    endo(a => apply(e(a)))
}

object Endo extends Endos {
  def apply[A](f: (=> A) => A): Endo[A] =
    endo(f)
}

trait Endos {
  def endo[A](f: (=> A) => A): Endo[A] = new Endo[A] {
    def apply(a: => A): A =
      f(a)
  }

  def constEndo[A](a: => A): Endo[A] = endo(_ => a)

  def idEndo[A]: Endo[A] = endo(z => z)

  implicit def EndoTo[A](e: Endo[A]): (=> A) => A =
    a => e.apply(a)
}
