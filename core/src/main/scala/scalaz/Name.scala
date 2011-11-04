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
    def pure[A](a: => A) = Name(a)

    override def map[A, B](fa: Name[A])(f: (A) => B) = Name(f(fa.value))
    def bind[A,B](v: Name[A])(f: A => Name[B]): Name[B] = f(v.value)
    def cojoin[A](a: Name[A]): Name[Name[A]] = Name(a)
    def copure[A](p: Name[A]): A = p.value
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
    def pure[A](a: => A) = Need(a)
    override def map[A, B](fa: Need[A])(f: A => B) = Need(f(fa.value))
    def bind[A, B](v: Need[A])(f: A => Need[B]): Need[B] = f(v.value)
    def cojoin[A](a: Need[A]): Need[Need[A]] = Need(a)
    def copure[A](p: Need[A]): A = p.value
  }
}

object Value {
  implicit val value = new Monad[Value] with Comonad[Value] with Cobind.FromCojoin[Value] {
    def pure[A](a: => A) = Value(a)
    def bind[A, B](v: Value[A])(f: A => Value[B]): Value[B] = f(v.value)
    def cojoin[A](a: Value[A]): Value[Value[A]] = Value(a)
    def copure[A](p: Value[A]): A = p.value
  }
}
