package scalaz

////
/**
 *
 */
////
trait Show[F]  { self =>
  ////
  def show(f: F): List[Char]
  def shows(f: F): String = show(f).mkString

  def xmlText(f: F): scala.xml.Text = scala.xml.Text(shows(f))

  // derived functions
  ////
  val showSyntax = new scalaz.syntax.ShowSyntax[F] {}
}

object Show {
  @inline def apply[F](implicit F: Show[F]): Show[F] = F

  ////

  def showFromToString[A]: Show[A] = new Show[A] {
    def show(f: A): List[Char] = f.toString.toList
  }

  /** For compatibility with Scalaz 6 */
  def showA[A]: Show[A] = showFromToString[A]

  def show[A](f: A => List[Char]): Show[A] = new Show[A] {
    def show(a: A): List[Char] = f(a)
  }

  implicit def showContravariant: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](r: Show[A])(f: (B) => A): Show[B] = new Show[B] {
      def show(b: B): List[Char] = r.show(f(b))
    }
  }

  ////
}

