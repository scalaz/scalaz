package scalaz

sealed trait Identity[A] {
  val value: A

  def pure[P[_]](implicit p: Pure[P]) = p pure value

  def |+|(a: => A)(implicit s: Semigroup[A]) = s append (value, a)

  def ===(a: A)(implicit e: Equal[A]) = e equal (value, a)

  def /=(a: A)(implicit e: Equal[A]) = !(===(a))

  def compare(a: A)(implicit o: Order[A]) = o order (value, a)

  def show(implicit s: Show[A]) = s.show(value)

  def shows(implicit s: Show[A]) = s.shows(value)

  def print(implicit s: Show[A]) = Console.print(shows)

  def println(implicit s: Show[A]) = Console.println(shows)

  def constantState[S, A](s: S) = State.constant(s, value)

  def text(implicit s: Show[A]) = xml.Text(s shows value)

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
