package scalaz

sealed trait Identity[A] {
  val value: A

  import Scalaz._
  import geo._

  def η[F[_]](implicit p: Pure[F]): F[A] = p pure value

  def σ: Dual[A] = value
  
  def ⊹(a: => A)(implicit s: Semigroup[A]): A = s append (value, a)

  def ≟(a: A)(implicit e: Equal[A]): Boolean = e equal (value, a)

  def ≠(a: A)(implicit e: Equal[A]): Boolean = !(≟(a))

  // using the implicit parameter ev here gives better compiler error messages for mistyped expressions like  1 assert_≟ "".
  // the simpler signature is def assert_≟(b: A)(implicit e: Equal[A], s: Show[A])
  def assert_≟[B](b: B)(implicit e: Equal[A], s: Show[A], ev: B <:< A) = if(≠(b)) error(shows + " ≠ " + ev(b).shows)

  def ?|?(a: A)(implicit o: Order[A]): Ordering = o order (value, a)

  def ≤(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) != GT

  def ≥(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) != LT

  def ≨(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) == LT

  def ≩(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) == GT

  def ≮(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) != LT

  def ≯(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) != GT

  def ≰(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) == GT

  def ≱(a: A)(implicit o: Order[A]): Boolean = o.order(value, a) == LT

  def show(implicit s: Show[A]): List[Char] = s.show(value)

  def shows(implicit s: Show[A]): String = s.show(value).mkString

  def print(implicit s: Show[A]): Unit = Console.print(shows)

  def println(implicit s: Show[A]): Unit = Console.println(shows)

  def text(implicit s: Show[A]): xml.Text = xml.Text(value.shows)

  def <===>(a: A)(implicit m: MetricSpace[A]): Int = m distance (value, a)

  def constantState[S](s: => S): State[S, A] = Scalaz.state((_: S) => (s, value))

  def state[S]: State[S, A] = Scalaz.state((_: S, value))

  def unfold[M[_], B](f: A => Option[(B, A)])(implicit p: Pure[M], m: Monoid[M[B]]): M[B] = f(value) match {
    case None => m.zero
    case Some((b, a)) => b.η ⊹ a.unfold(f)
  }

  def replicate[M[_]](n: Int)(implicit p: Pure[M], m: Monoid[M[A]]): M[A] =
    if (n <= 0) ∅
    else value.η ⊹ replicate(n - 1)

  def repeat[M[_]](implicit p: Pure[M], m: Monoid[M[A]]): M[A] = value.η ⊹ repeat

  def iterate[M[_]](f: A => A)(implicit p: Pure[M], m: Monoid[M[A]]): M[A] =
    value.η ⊹ f(value).iterate(f)

  def zipper: Zipper[A] = Scalaz.zipper(Stream.empty, value, Stream.empty)

  def unfoldTree[B](f: A => (B, () => Stream[A])): Tree[B] = f(value) match {
    case (a, bs) => node(a, bs.apply.unfoldForest(f))
  }

  def unfoldTreeM[B, M[_]](f: A => M[(B, Stream[A])])(implicit m: Monad[M]): M[Tree[B]] = {
    m.bind(f(value), (abs: (B, Stream[A])) =>
        m.bind(abs._2.unfoldForestM[B, M](f), (ts: Stream[Tree[B]]) =>
            m.pure(node(abs._1, ts))))
  }

  def success[X]: Validation[X, A] = Scalaz.success(value)

  def fail[X]: Validation[A, X] = Scalaz.failure(value)

  def dlist: DList[A] = Scalaz.dlist(value :: (_: List[A]))

  def nel: NonEmptyList[A] = Scalaz.nel(value, Nil)

  def toRadians(implicit r: Radians[A]): Double = r.toRadians(value)

  def doWhile(f: A => A, p: A => Boolean): A = {
    val x = f(value)
    if(p(x)) x.doWhile(f, p) else x
  }

  def whileDo(f: A => A, p: A => Boolean): A =
    if(p(value)) value.whileDo(f, p) else value

  override def toString = value.toString

  override def hashCode = value.hashCode

  override def equals(o: Any) = o.isInstanceOf[Identity[_]] && value == o.asInstanceOf[Identity[_]].value
}

trait Identitys {
  implicit def IdentityTo[A](x: A): Identity[A] = new Identity[A] {
    val value = x
  }

  implicit def IdentityFrom[A](x: Identity[A]): A = x.value

  val unital = IdentityTo(())
}
