package scalaz
package iteratee

import Iteratee._
import Ordering._

trait EnumerateeT[X, O, I, F[_]] { self =>
  def apply[A]: StepT[X, I, F, A] => IterateeT[X, O, F, StepT[X, I, F, A]]

  def run(enum: EnumeratorT[X, O, F])(implicit M: Monad[F]): EnumeratorT[X, I, F] = {
    new EnumeratorT[X, I, F] {
      def apply[A] = (s: StepT[X, I, F, A]) => iterateeT((self[A](s) &= enum).run(x => err[X, I, F, A](x).value))
    }
  }
}

object EnumerateeT extends EnumerateeTFunctions

trait EnumerateeTFunctions {
  /**
   * Applies a function to each input element and feeds the resulting outputs to the inner iteratee.
   */
  def map[X, O, I, F[_] : Monad](f: O => I): EnumerateeT[X, O, I, F] = mapErrorOr(o => Right(f(o)))

  /**
   * Applies a function to each input element and, if the result is a right feeds the resulting outputs to the inner
   * iteratee, otherwise throws an error.
   */
  def mapErrorOr[X, O, I, F[_] : Monad](f: O => Either[X, I]): EnumerateeT[X, O, I, F] = 
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

  def flatMap[X, O, I, F[_]: Monad](f: O => EnumeratorT[X, I, F]): EnumerateeT[X, O, I, F] = 
    new EnumerateeT[X, O, I, F] {
      def apply[A] = {
        def loop(step: StepT[X, I, F, A]): IterateeT[X, O, F, StepT[X, I, F, A]] = {
          step.fold(
            cont = contf => cont[X, O, F, StepT[X, I, F, A]] {
              (_: Input[O]).map(e => f(e)).fold(
                el    = en => en.apply(step) >>== loop,
                empty = contf(emptyInput) >>== loop,
                eof   = done(step, emptyInput)
              )
            },
            done = (a, _) => done(sdone(a, emptyInput), emptyInput),
            err  = x => err(x)
          )
        }

        loop
      }
    }

  def collect[X, O, I, F[_] : Monad](pf: PartialFunction[O, I]): EnumerateeT[X, O, I, F] = collectErrorOr(pf andThen (i => Right(i)))

  def collectErrorOr[X, O, I, F[_] : Monad](pf: PartialFunction[O, Either[X, I]]): EnumerateeT[X, O, I, F] = 
    new EnumerateeT[X, O, I, F] {
      def apply[A] = {
        def loop = step andThen cont[X, O, F, StepT[X, I, F, A]]
        def step: (Input[I] => IterateeT[X, I, F, A]) => (Input[O] => IterateeT[X, O, F, StepT[X, I, F, A]]) = {
          k => in =>
            in(
              el = e => if (pf.isDefinedAt(e)) pf(e).fold(err(_), i => k(elInput(i)) >>== doneOr(loop)) else cont(step(k))
              , empty = cont(step(k))
              , eof = done(scont(k), in)
            )
        }

        doneOr(loop)
      }
    }

  def filter[X, E, F[_] : Monad](p: E => Boolean): EnumerateeT[X, E, E, F] = 
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

  /**
   * Uniqueness filter. Assumes that the input enumerator is already sorted.
   */
  def uniq[X, E: Order, F[_]: Monad]: EnumerateeT[X, E, E, F] = 
    new EnumerateeT[X, E, E, F] {
      def apply[A] = {
        def step(s: StepT[X, E, F, A], last: Input[E]): IterateeT[X, E, F, A] = 
          s mapCont { k => 
            cont { in =>
              val inr = in.filter(e => last.forall(l => Order[E].order(e, l) != EQ))
              k(inr) >>== (step(_, in))
            }
          }

        s => step(s, emptyInput).map(sdone(_, emptyInput))
      }
    }
    
  /**
   * Zips with the count of elements that have been encountered.
   */
  def zipWithIndex[X, E, F[_]: Monad]: EnumerateeT[X, E, (E, Long), F] = 
    new EnumerateeT[X, E, (E, Long), F] {
      def apply[A] = {
        type StepEl = Input[(E, Long)] => IterateeT[X, (E, Long), F, A] 
        def loop(i: Long) = (step(_: StepEl, i)) andThen (cont[X, E, F, StepT[X, (E, Long), F, A]])
        def step(k: StepEl, i: Long): (Input[E] => IterateeT[X, E, F, StepT[X, (E, Long), F, A]]) = {
          (in: Input[E]) =>
            in.map(e => (e, i)).fold(
              el = e => k(elInput(e)) >>== doneOr(loop(i + 1))
              , empty = cont(step(k, i))
              , eof = done(scont(k), in)
            )
        }

        doneOr(loop(0))
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

