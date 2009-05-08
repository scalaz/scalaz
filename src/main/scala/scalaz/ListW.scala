package scalaz

sealed trait ListW[A] {
  val value: List[A]

  def string(f: A => Char) = value map f mkString

  def stringj(f: A => List[Char]) = value flatMap f mkString

  import S._

  def intersperse(a: A): List[A] = value match {
    case Nil => Nil
    case x :: Nil => x :: Nil
    case h :: t => h :: a :: t.intersperse(a)
  }

  def intercalate(as: List[A]): List[A] = value match {
    case Nil => Nil
    case x :: Nil => x :: Nil
    case h :: t => h :: as ::: t.intercalate(as)
  }

  def nel = value match {
    case Nil => None
    case h :: t => Some(NonEmptyList.nel(h, t))
  }

  def <^>[B](f: NonEmptyList[A] => B)(implicit z: Zero[B]) = value match {
    case Nil => z.zero
    case h :: t => f(NonEmptyList.nel(h, t))
  }

  def choose = (0 >--> value.length) >>= (_ <=- (value: _*))
}

object ListW {
  implicit def ListTo[A](as: List[A]): ListW[A] = new ListW[A] {
    val value = as
  }

  implicit def ListFrom[A](as: ListW[A]): List[A] = as.value
}
