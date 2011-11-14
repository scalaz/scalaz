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

  def text(f: F): xml.Text = xml.Text(shows(f))

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

  ////
}

