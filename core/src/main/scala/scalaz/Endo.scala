package scalaz

/** Endomorphisms.  They have special properties among functions, so
  * are captured in this newtype.
  *
  * @param run The captured function.
  */
final case class Endo[A](run: A => A) {
  final def apply(a: A): A = run(a)

  /** Do `other`, than call myself with its result. */
  final def compose(other: Endo[A]): Endo[A] = Endo.endo(run compose other.run)

  /** Call `other` with my result. */
  final def andThen(other: Endo[A]): Endo[A] = other compose this
}

object Endo extends EndoInstances with EndoFunctions

sealed abstract class EndoInstances {

  /** Endo forms a monoid where `zero` is the identity endomorphism
    * and `append` composes the underlying functions. */
  implicit def endoInstance[A]: Monoid[Endo[A]] = new Monoid[Endo[A]] {
    def append(f1: Endo[A], f2: => Endo[A]) = f1 compose f2
    def zero = Endo.idEndo
  }
  implicit def endoInstances: Zip[Endo] with Unzip[Endo] = new Zip[Endo] with Unzip[Endo] {
    def zip[A, B](a: => Endo[A], b: => Endo[B]) =
      Endo {
        case (x, y) => (a(x), b(y))
      }

    // CAUTION: cheats with null
    def unzip[A, B](a: Endo[(A, B)]) =
      (Endo(x => a((x, null.asInstanceOf[B]))._1), Endo(x => a((null.asInstanceOf[A], x))._2))
  }
}

trait EndoFunctions {
  /** Alias for `Endo.apply`. */
  final def endo[A](f: A => A): Endo[A] = Endo(f)

  /** Always yield `a`. */
  final def constantEndo[A](a: => A): Endo[A] = endo[A](_ => a)

  /** Alias for `Monoid[Endo[A]].zero`. */
  final def idEndo[A]: Endo[A] = endo[A](a => a)

  import Isomorphism.{IsoSet, IsoFunctorTemplate}

  implicit def IsoEndo[A] = new IsoSet[Endo[A], A => A] {
    def to: (Endo[A]) => A => A = _.run
    def from: (A => A) => Endo[A] = endo
  }

  implicit def IsoFunctorEndo = new IsoFunctorTemplate[Endo, ({type λ[α]=(α => α)})#λ] {
    def to[A](fa: Endo[A]): A => A = fa.run
    def from[A](ga: A => A): Endo[A] = endo(ga)
  }
}
