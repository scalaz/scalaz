package scalaz

import std.option.{none, some}

import Free.Suspend

/**
 * Inject type class as described in "Data types a la carte" (Swierstra 2008).
 *
 * @see [[http://www.staff.science.uu.nl/~swier004/Publications/DataTypesALaCarte.pdf]]
 */
sealed abstract class :<:[F[_]: Functor, G[_]: Functor] {
  def inj[A](fa: F[A]): G[A]
  def prj[A](ga: G[A]): Option[F[A]]
}

sealed trait InjectInstances {
  implicit def reflexiveInjectInstance[F[_]: Functor] =
    new (F :<: F) {
      def inj[A](fa: F[A]) = fa
      def prj[A](ga: F[A]) = some(ga)
    }

  implicit def leftInjectInstance[F[_]: Functor, G[_]: Functor] =
    new (F :<: ({type λ[α] = Coproduct[F, G, α]})#λ) {
      def inj[A](fa: F[A]) = Coproduct.leftc(fa)
      def prj[A](ga: ({type λ[α] = Coproduct[F, G, α]})#λ[A]) = ga.run.fold(some(_), _ => none)
    }

  implicit def rightInjectInstance[F[_]: Functor, G[_]: Functor, H[_]: Functor]
    (implicit I: F :<: G) =
      new (F :<: ({type λ[α] = Coproduct[H, G, α]})#λ) {
        def inj[A](fa: F[A]) = Coproduct.rightc(I.inj(fa))
        def prj[A](ga: ({type λ[α] = Coproduct[H, G, α]})#λ[A]) = ga.run.fold(_ => none, I.prj(_))
      }
}

sealed trait InjectFunctions {
  def inject[F[_]: Functor, G[_]: Functor, A](ga: G[Free[F, A]])(implicit I: G :<: F): Free[F, A] =
    Suspend[F, A](I.inj(ga))

  def match_[F[_]: Functor, G[_]: Functor, A](fa: Free[F, A])(implicit I: G :<: F): Option[G[Free[F, A]]] =
    fa.resume.fold(I.prj(_), _ => none)
}

object :<: extends InjectInstances with InjectFunctions
