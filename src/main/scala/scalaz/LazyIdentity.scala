package scalaz

sealed trait LazyIdentity[A] {
  def value: A

  def throws = try { Right(value) } catch { case e => Left(e) }
}

object LazyIdentity {
  implicit def LazyIdentityTo[A](a: => A) = new LazyIdentity[A] {
    def value = a
  }

  def id[A](f: () => A) = LazyIdentityTo(f())

  implicit def LazyIdentityFrom[A](i: LazyIdentity[A]) = i.value
}