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

  implicit val showContravariant: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](r: Show[A])(f: B => A): Show[B] = new Show[B] {
      override def show(b: B): Cord = r.show(f(b))
    }
  }

  import Isomorphism.<=>

  def fromIso[F, G](D: F <=> G)(implicit S: Show[G]): Show[F] =
    new IsomorphismShow[F, G] {
      override implicit def G: Show[G] = S
      override def iso: F <=> G = D
    }

  final case class Shows(override val toString: String) extends AnyVal
  object Shows {
    implicit def mat[A](x: A)(implicit S: Show[A]): Shows = Shows(S.shows(x))
  }

  final case class ShowInterpolator(sc: StringContext) extends AnyVal {
    def show(args: Shows*): String = sc.s(args: _*)
  }

  ////
}
