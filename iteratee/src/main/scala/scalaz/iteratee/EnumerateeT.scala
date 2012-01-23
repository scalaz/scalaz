package scalaz
package iteratee

import Iteratee._

trait EnumerateeT[X, O, I, F[_]] {
  def apply[A]: StepT[X, I, F, A] => IterateeT[X, O, F, StepT[X, I, F, A]]
}

object EnumerateeT extends EnumerateeTFunctions

trait EnumerateeTFunctions {
  /**
   * Applies a function to each input element and feeds the resulting outputs to the inner iteratee.
   */
  def map[X, O, I, F[_] : Pointed : Bind](f: O => I): EnumerateeT[X, O, I, F] = mapErrorOr(o => Right(f(o)))

  /**
   * Applies a function to each input element and, if the result is a right feeds the resulting outputs to the inner
   * iteratee, otherwise throws an error.
   */
  def mapErrorOr[X, O, I, F[_] : Pointed : Bind](f: O => Either[X, I]): EnumerateeT[X, O, I, F] = 
    new EnumerateeT[X, O, I, F] {
      def apply[A] = {
        def loop = step andThen cont[X, O, F, StepT[X, I, F, A]]
        def step: (Input[I] => IterateeT[X, I, F, A]) => (Input[O] => IterateeT[X, O, F, StepT[X, I, F, A]]) = {
          k => in =>
            in(
              el = e => f(e).fold(err(_), i => k(elInput(i)) >>== doneOr(loop))
              , empty = cont(step(k))
              , eof = done(scont(k), in)
            )
        }

        doneOr(loop)
      }
    }

  def filter[X, E, F[_] : Pointed : Bind](p: E => Boolean): EnumerateeT[X, E, E, F] = 
    new EnumerateeT[X, E, E, F] {
      def apply[A] = {
        def loop = step andThen cont[X, E, F, StepT[X, E, F, A]]
        def step: (Input[E] => IterateeT[X, E, F, A]) => (Input[E] => IterateeT[X, E, F, StepT[X, E, F, A]]) = {
          k => in =>
            in(
              el = e =>
                if (p(e)) k(in) >>== doneOr(loop)
                else cont(step(k))
              , empty = cont(step(k))
              , eof = done(scont(k), in)
            )
        }

        doneOr(loop)
      }
    }

  def group[X, E, F[_], G[_]](n: Int)(implicit F: Pointed[F], FE: Monoid[F[E]], G: Monad[G], G1: CoPointed[G]): EnumerateeT[X, E, F[E], G] = 
    new EnumerateeT[X, E, F[E], G] {
      def apply[A] = take[X, E, F](n).up[G].sequenceI.apply[A]
    }

  def splitOn[X, E, F[_], G[_]](p: E => Boolean)(implicit F: Pointed[F], FE: Monoid[F[E]], G: Monad[G], G1: CoPointed[G]): EnumerateeT[X, E, F[E], G] = 
    new EnumerateeT[X, E, F[E], G] {
      def apply[A] = {
        (takeWhile[X, E, F](p).up[G] flatMap (xs => drop[X, E, G](1).map(_ => xs))).sequenceI.apply[A]
      }
    }


  def doneOr[X, O, I, F[_] : Pointed, A](f: (Input[I] => IterateeT[X, I, F, A]) => IterateeT[X, O, F, StepT[X, I, F, A]]): StepT[X, I, F, A] => IterateeT[X, O, F, StepT[X, I, F, A]] = {
    (s: StepT[X, I, F, A]) => s.mapContOr(k => f(k), done(s, emptyInput))
  }
}

