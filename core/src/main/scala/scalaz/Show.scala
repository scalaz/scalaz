package scalaz

////
/**
 * A typeclass for conversion to textual representation, done via
 * [[scalaz.Cord]] for efficiency.
 */
////
trait Show[F]  { self =>
  ////
  def show(f: F): Cord
  def shows(f: F): String = show(f).shows

  // derived functions
  ////
  val showSyntax = new scalaz.syntax.ShowSyntax[F] { def F = Show.this }
}

object Show {
  @inline def apply[F](implicit F: Show[F]): Show[F] = F

  import Isomorphism._

  def fromIso[F, G](D: F <=> G)(implicit M: Show[G]): Show[F] =
    new IsomorphismShow[F, G] {
      override def G: Show[G] = M
      override def iso: F <=> G = D
    }

  ////

  def showFromToString[A]: Show[A] = new Show[A] {
    override def show(f: A): Cord = Cord(shows(f))
    override def shows(f: A): String = f.toString
  }

  /** For compatibility with Scalaz 6 */
  def showA[A]: Show[A] = showFromToString[A]

  def show[A](f: A => Cord): Show[A] = new Show[A] {
    override def show(a: A): Cord = f(a)
  }

  def shows[A](f: A => String): Show[A] = new Show[A] {
    override def show(f: A): Cord = Cord(shows(f))
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
  object Shows {
    implicit def mat[A](x: A)(implicit S: Show[A]): Shows = Shows(S.shows(x))
  }

  final case class ShowInterpolator(sc: StringContext) extends AnyVal {
    def show(args: Shows*): String = sc.s(args: _*)
  }
  ////
}

trait IsomorphismShow[F, G] extends Show[F] {
  implicit def G: Show[G]
  ////
  import Isomorphism._

  def iso: F <=> G

  override def show(f: F): Cord =
    G.show(iso.to(f))
  ////
}
