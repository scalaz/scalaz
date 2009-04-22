package scalaz

sealed trait Identity[A] {
  val value: A

  import S._

  def pure[P[_]](implicit p: Pure[P]) = p pure value

  def |+|(a: => A)(implicit s: Semigroup[A]) = s append (value, a)

  def ===(a: A)(implicit e: Equal[A]) = e equal (value, a)

  def /=(a: A)(implicit e: Equal[A]) = !(===(a))

  def compare(a: A)(implicit o: Order[A]) = o order (value, a)

  def show(implicit s: Show[A]) = s.show(value)

  def shows(implicit s: Show[A]) = s.shows(value)

  def print(implicit s: Show[A]) = Console.print(shows)

  def println(implicit s: Show[A]) = Console.println(shows)

  def text(implicit s: Show[A]) = xml.Text(s shows value)

  def constantState[S, A](s: S) = State.state((_: S) => (s, value))

  def state[S] = State.state((_: S, value))

  sealed trait Unfold[M[_]] {
    def apply[B](f: A => Option[(B, A)])(implicit p: Pure[M], m: Monoid[M[B]]): M[B]
  }

  def unfold[M[_]]: Unfold[M] = new Unfold[M] {
    def apply[B](f: A => Option[(B, A)])(implicit p: Pure[M], m: Monoid[M[B]]) = f(value) match {
      case None => m.zero
      case Some((b, a)) => b.pure[M] |+| a.unfold[M](f)
    }
  }

  def replicate[M[_]](n: Int)(implicit p: Pure[M], m: Monoid[M[A]]): M[A] =
    if(n <= 0) m.zero
    else value.pure[M] |+| replicate[M](n - 1)

  def repeat[M[_]](implicit p: Pure[M], m: Monoid[M[A]]): M[A] = value.pure[M] |+| repeat[M]   

  override def toString = value.toString

  override def hashCode = value.hashCode

  override def equals(o: Any) = o.isInstanceOf[Identity[_]] && value == o.asInstanceOf[Identity[_]].value
}

object Identity {
  implicit def id[A](x: A) = new Identity[A] {
    val value = x
  }

  val u = id(())
}
