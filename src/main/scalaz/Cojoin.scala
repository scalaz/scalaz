package scalaz

trait Cojoin[M[_]] {
  def cojoin[A](a: M[A]): M[M[A]]
}

object Cojoin {
  implicit def IdentityCojoin = new Cojoin[Identity] {
    def cojoin[A](a: Identity[A]) = Identity.id(a)
  }
}