package scalaz

/** Call by name */
sealed abstract class Name[+A] {
  def value: A
}

/** Call by need */
sealed abstract class Need[+A] extends Name[A]

/** Call by value */
final case class Value[+A](value: A) extends Need[A]

object Name { 
  def apply[A](a: => A) = new Name[A] {
    def value = a
  }
  def unapply[A](v: Name[A]): Option[A] = Some(v.value)

  implicit val name = new Monad[Name] with Comonad[Name] with Cobind.FromCojoin[Name] {
    def point[A](a: => A) = Name(a)

    override def map[A, B](fa: Name[A])(f: (A) => B) = Name(f(fa.value))
    def bind[A,B](v: Name[A])(f: A => Name[B]): Name[B] = f(v.value)
    def cojoin[A](a: Name[A]): Name[Name[A]] = Name(a)
    def copoint[A](p: Name[A]): A = p.value
  }
  implicit def nameEqual[A: Equal]: Equal[Name[A]] = new Equal[Name[A]] {
    def equal(a1: Name[A], a2: Name[A]): Boolean = Equal[A].equal(a1.value, a2.value)
  }
}

object Need {
  def apply[A](a: => A) = {
    lazy val value0: A = a
    new Need[A] {
      def value = value0
    }
  }
  def unapply[A](x: Need[A]): Option[A] = Some(x.value)

  implicit val need = new Monad[Need] with Comonad[Need] with Cobind.FromCojoin[Need] {
    def point[A](a: => A) = Need(a)
    override def map[A, B](fa: Need[A])(f: A => B) = Need(f(fa.value))
    def bind[A, B](v: Need[A])(f: A => Need[B]): Need[B] = f(v.value)
    def cojoin[A](a: Need[A]): Need[Need[A]] = Need(a)
    def copoint[A](p: Need[A]): A = p.value
  }
  implicit def needEqual[A: Equal]: Equal[Need[A]] = new Equal[Need[A]] {
    def equal(a1: Need[A], a2: Need[A]): Boolean = Equal[A].equal(a1.value, a2.value)
  }
}

object Value {
  implicit val value = new Monad[Value] with Comonad[Value] with Cobind.FromCojoin[Value] {
    def point[A](a: => A) = Value(a)
    def bind[A, B](v: Value[A])(f: A => Value[B]): Value[B] = f(v.value)
    def cojoin[A](a: Value[A]): Value[Value[A]] = Value(a)
    def copoint[A](p: Value[A]): A = p.value
  }
  implicit def valueEqual[A: Equal]: Equal[Value[A]] = new Equal[Value[A]] {
    def equal(a1: Value[A], a2: Value[A]): Boolean = Equal[A].equal(a1.value, a2.value)
  }
}
