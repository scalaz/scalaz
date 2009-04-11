package scalaz

sealed trait Order[-A] {
  def order(a1: A, a2: A): Ordering

  def equal = Equal.equal[A](order(_, _) == EQ)
}

object Order {
  def order[A](f: (A, A) => Ordering): Order[A] = new Order[A] {
    def order(a1: A, a2: A) = f(a1, a2)
  }

  implicit def ordered[A](a: A)(implicit o: Order[A]): Ordered[A] = new Ordered[A] {
    def compare(aa: A) = o.order(a, aa).toInt
  }

  implicit val StringOrder: Order[String] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)
}
