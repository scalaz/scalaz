package scalaz

////
/**
 * A typeclass for conversion to textual representation, done via
 * [[scalaz.Cord]] for efficiency.
 */
////
trait Show[F]  { self =>
  ////
  def show(f: F): Cord = Cord(shows(f))
  def shows(f: F): String = show(f).toString

  def xmlText(f: F): scala.xml.Text = scala.xml.Text(shows(f))

  // derived functions
  ////
  val showSyntax = new scalaz.syntax.ShowSyntax[F] { def F = Show.this }
}

object Show {
  @inline def apply[F](implicit F: Show[F]): Show[F] = F

  ////

  def showFromToString[A]: Show[A] = new Show[A] {
    override def shows(f: A): String = f.toString
  }

  /** For compatibility with Scalaz 6 */
  def showA[A]: Show[A] = showFromToString[A]

  def show[A](f: A => Cord): Show[A] = new Show[A] {
    override def show(a: A): Cord = f(a)
  }

  def shows[A](f: A => String): Show[A] = new Show[A] {
    override def shows(a: A): String = f(a)
  }

  implicit def showContravariant: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](r: Show[A])(f: (B) => A): Show[B] = new Show[B] {
      override def show(b: B): Cord = r.show(f(b))
    }
  }

  ////
}
