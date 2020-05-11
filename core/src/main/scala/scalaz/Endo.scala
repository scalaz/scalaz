package scalaz

import scalaz.Isomorphism.{<~>, <=>}

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

/** Endomorphisms using by-name evaluation.  They have special properties among functions, so
  * are captured in this newtype.
  *
  * @param run The captured function.
  */
final case class EndoByName[A](run: (=>A) => A) {
  final def apply(a: => A): A = run(a)

  /** Do `other`, than call myself with its result. */
  final def compose(other: => EndoByName[A]): EndoByName[A] = EndoByName(x => run(other.run(x)))

  /** Call `other` with my result. */
  final def andThen(other: EndoByName[A]): EndoByName[A] = other compose this
}

object Endo extends EndoInstances {
  /** Alias for `Endo.apply`. */
  final def endo[A](f: A => A): Endo[A] = Endo(f)

  /** Alias for `EndoByName.apply`. */
  final def endoByName[A](f: (=>A) => A): EndoByName[A] = EndoByName(f)

  /** Always yield `a`. */
  final def constantEndo[A](a: => A): Endo[A] = endo[A](_ => a)

  /** Always yield `a`. */
  final def constantEndoByName[A](a: => A): EndoByName[A] = EndoByName(_ => a)

  /** Alias for `Monoid[Endo[A]].zero`. */
  final def idEndo[A]: Endo[A] = endo[A](a => a)

  /** Alias for `Monoid[EndoByName[A]].zero`. */
  final def idEndoByName[A]: EndoByName[A] = EndoByName[A](a => a)

  import Isomorphism.{IsoSet, IsoFunctorTemplate}

  def IsoEndo[A]: Endo[A] <=> (A => A) = new IsoSet[Endo[A], A => A] {
    def to: (Endo[A]) => A => A = _.run
    def from: (A => A) => Endo[A] = endo
  }

  val IsoFunctorEndo: Endo <~> λ[α => α => α] = new IsoFunctorTemplate[Endo, λ[α => α => α]] {
    def to[A](fa: Endo[A]): A => A = fa.run
    def from[A](ga: A => A): Endo[A] = endo(ga)
  }
}

object EndoByName extends EndoByNameInstances

sealed abstract class EndoInstances {

  /** Endo forms a monoid where `zero` is the identity endomorphism
    * and `append` composes the underlying functions. */
  implicit def endoInstance[A]: Monoid[Endo[A]] = new Monoid[Endo[A]] {
    def append(f1: Endo[A], f2: => Endo[A]) = f1 compose f2
    def zero = Endo.idEndo
  }
  implicit val endoInstances: Zip[Endo] with Unzip[Endo] with InvariantFunctor[Endo] = new Zip[Endo] with Unzip[Endo] with InvariantFunctor[Endo] {
    def xmap[A, B](fa: Endo[A], f: A => B, g: B => A) =
      Endo.endo(g andThen fa.run andThen f)

    def zip[A, B](a: => Endo[A], b: => Endo[B]) =
      Endo {
        case (x, y) => (a(x), b(y))
      }

    // CAUTION: cheats with null
    def unzip[A, B](a: Endo[(A, B)]) =
      (Endo(x => a((x, null.asInstanceOf[B]))._1), Endo(x => a((null.asInstanceOf[A], x))._2))
  }
}


sealed abstract class EndoByNameInstances {

  /** Endo forms a monoid where `zero` is the identity endomorphism
    * and `append` composes the underlying functions. */
  implicit def endoInstance[A]: Monoid[EndoByName[A]] = new Monoid[EndoByName[A]] {
    def append(f1: EndoByName[A], f2: => EndoByName[A]) = f1 compose f2
    def zero = Endo.idEndoByName
  }
  implicit val endoInstances: Zip[EndoByName] with Unzip[EndoByName] with InvariantFunctor[EndoByName] = new Zip[EndoByName] with Unzip[EndoByName] with InvariantFunctor[EndoByName] {
    def xmap[A, B](fa: EndoByName[A], f: A => B, g: B => A) =
      Endo.endoByName(a => f(fa(g(a))))

    def zip[A, B](a: => EndoByName[A], b: => EndoByName[B]) =
      EndoByName {
        case (x, y) => (a(x), b(y))
      }

    // CAUTION: cheats with null
    def unzip[A, B](a: EndoByName[(A, B)]) =
      (EndoByName(x => a((x, null.asInstanceOf[B]))._1), EndoByName(x => a((null.asInstanceOf[A], x))._2))
  }
}
