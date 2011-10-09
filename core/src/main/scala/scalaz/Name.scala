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

  implicit val name: Monad[Name] = new Monad[Name] {
    def pure[A](a: => A) = Name(a)

    override def map[A, B](fa: Name[A])(f: (A) => B) = Name(f(fa.value))

    def bind[A,B](v: Name[A])(f: A => Name[B]): Name[B] = f(v.value)
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

  implicit val need: Monad[Need] = new Monad[Need] {
    def pure[A](a: => A) = Need(a)

    override def map[A, B](fa: Need[A])(f: A => B) = Need(f(fa.value))

    def bind[A, B](v: Need[A])(f: A => Need[B]): Need[B] = f(v.value)
  }
}

object Value {
  implicit val value: Monad[Value] = new Monad[Value] {
    def pure[A](a: => A) = Value(a)

    def bind[A, B](v: Value[A])(f: A => Value[B]): Value[B] = f(v.value)
  }
}
