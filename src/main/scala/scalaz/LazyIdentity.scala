package scalaz

sealed trait LazyIdentity[A] {
  def value: A

  def onull = {
    val v = value
    if(v == null) None else Some(v) 
  }
}

object LazyIdentity {
  implicit def LazyIdentityTo[A](a: => A) = new LazyIdentity[A] {
    def value = a
  }

  def id[A](f: () => A) = LazyIdentityTo(f())

  implicit def LazyIdentityFrom[A](i: LazyIdentity[A]) = i.value
}
