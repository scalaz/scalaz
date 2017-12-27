package scalaz
package data

import scalaz.Prelude._
import scalaz.data.Disjunction.{-\/, \/-}
import scalaz.typeclass.{Axioms, IsInjective, Proposition}

final class Incomparable[A, B]
(val notLessOrEqual: ¬[A <~< B],
 val notGreaterOrEqual: ¬[B <~< A]) { ab =>
  import Incomparable._

  def notEqual: A =!= B = equal => notLessOrEqual(equal.toAs)
  def notLess: ¬[A </< B] = ineq => notLessOrEqual(ineq.conformity)
  def notGreater: ¬[B </< A] = ineq => notGreaterOrEqual(ineq.conformity)

  def lift[F[_]](implicit F: IsInjective[F]): F[A] >!< F[B] =
    F.incomparable[A, B](ab)

  def flip: B >!< A = witness(notGreaterOrEqual, notLessOrEqual)
}
object Incomparable {
  def witness[A, B](notBelow: ¬[A <~< B], notAbove: ¬[B <~< A]): Incomparable[A, B] =
    new Incomparable[A, B](notBelow, notAbove)

  /**
    * ¬(a ~ b) ⋀ ¬(a < b) ⋀ ¬(b < a)
    */
  def witness1[A, B](notEqual: A =!= B, notLess: ¬[A </< B], notGreater: ¬[B </< A]): A >!< B =
    Axioms.subtyping(notEqual).map {
      case \/-(ncmp) => ncmp
      case -\/(\/-(lt)) => notLess(lt)
      case -\/(-\/(gt)) => notGreater(gt)
    }.proved

  def witnessNot[A, B](ev: ¬¬[(B <~< A) \/ (A <~< B)]): ¬[A >!< B] = ab => ev.run {
    case \/-(below) => ab.notLessOrEqual(below)
    case -\/(above) => ab.notGreaterOrEqual(above)
  }

  implicit def proposition[A, B]: Proposition[Incomparable[A, B]] =
    (Proposition[¬[A <~< B]] zip Proposition[¬[B <~< A]]).isomap(Iso.unsafeTotal(
      p => witness(p._1, p._2),
      p => (p.notLessOrEqual, p.notGreaterOrEqual)
    ))

  def irreflexive[A](ev: A >!< A): Void =
    ev.notEqual(Is.refl)
}