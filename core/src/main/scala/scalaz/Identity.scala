package scalaz

sealed trait Identity[A] {
  def value: A

  def map[B](f: A => B): Identity[B] = new Identity[B] {
    def value = f(Identity.this.value)
  }
}

object Identity extends Identitys {
  def apply[A]: (=> A) => Identity[A] =
    id[A]
}

trait Identitys {
  def id[A]: (=> A) => Identity[A] = v => new Identity[A] {
    def value = v
  }

}
