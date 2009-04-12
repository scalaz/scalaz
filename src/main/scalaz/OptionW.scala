package scalaz

sealed trait OptionW[+A] {
  val value: Option[A]

  sealed trait Fold[X] {
    def none(s: A => X): X
  }

  def some[X](s: A => X) = new {
    def none(n: => X) = value match {
      case None => n
      case Some(a) => s(a)
    }
  }

  sealed trait Conditional[X] {
    def |(n: => X): X
  }

  def ?[X](s: => X) = new Conditional[X] {
    def |(n: => X) = value match {
      case None => n
      case Some(_) => s
    }
  }

  def ifNone(n: => Unit) = if(value.isEmpty) n

  def err(message: => String) = value getOrElse (error(message))

  def |[AA >: A](a: => AA) = value getOrElse a

  def toNull = value getOrElse null.asInstanceOf[A]

  def unary_~[AA >: A](implicit z: Zero[AA]) = value getOrElse z.zero
}

object OptionW {
  implicit def OptionTo[A](o: Option[A]) = new OptionW[A] {
    val value = o
  }

  implicit def OptionFrom[A](o: OptionW[A]) = o.value
}
