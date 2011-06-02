package scalaz

// formerly Need
sealed trait !*[A] {
  protected def v: A

  lazy val value = v

  import !*._

  def map[B](f: A => B): !*[B] =
    need(f(value))

  def flatMap[B](f: A => !*[B]): !*[B] =
    f(value)

  def foreach(f: A => Unit): Unit =
    f(value)

  def liftP[F[_]](implicit p: Pointed[F]): F[A] =
    p.point(value)
}

object !* extends !** {
  def apply[A](a: => A): !*[A] =
    need(a)
}

trait !** {
  def need[A](a: => A): !*[A] = new !*[A] {
    def v = a
  }
}
