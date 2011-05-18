package scalaz
package data

// formerly Name
sealed trait ~*[A] {
  def value: A

  import ~*._

  def map[B](f: A => B): ~*[B] =
    name(f(value))

  def flatMap[B](f: A => ~*[B]): ~*[B] =
    f(value)

  def foreach(f: A => Unit): Unit =
    f(value)

  def liftP[F[_]](implicit p: Pointed[F]): F[A] =
    p.point(value)
}

object ~* extends ~** {
  def apply[A](a: => A): ~*[A] =
    name(a)
}

trait ~** {
  def name[A](a: => A): ~*[A] = new ~*[A] {
    def value = a
  }
}
