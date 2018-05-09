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

  // scalaz-deriving provides a coherent n-arity derivers extending this
  private[scalaz] class ShowContravariant extends Contravariant[Show] {
    def contramap[A, B](r: Show[A])(f: B => A): Show[B] = new Show[B] {
      override def show(b: B): Cord = r.show(f(b))
    }
  }
  implicit val showContravariant: Contravariant[Show] = new ShowContravariant

  final case class Shows(override val toString: String) extends AnyVal
  object Shows extends Shows0 {
    implicit def mat[A](x: A)(implicit S: Show[A]): Shows = Shows(S.shows(x))
  }
  sealed abstract class Shows0 { this: Shows.type =>
    @annotation.implicitAmbiguous("Cannot use value of type ${A} in the `show` interpolator, as no `Show[${A}]` instance could be found")
    implicit def showsAmbig0[A](x: A): Shows = sys.error("showsAmbig0")
    implicit def showsAmbig1[A](x: A): Shows = sys.error("showsAmbig1")
  }

  final case class ShowInterpolator(sc: StringContext) extends AnyVal {
    def z(args: Shows*): String = sc.s(args: _*)
  }
  ////
}
