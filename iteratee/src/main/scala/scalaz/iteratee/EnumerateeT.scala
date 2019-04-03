package scalaz
package iteratee

import Iteratee._
import Ordering._

trait EnumerateeT[O, I, F[_]] { self =>
  def apply[A]: StepT[I, F, A] => IterateeT[O, F, StepT[I, F, A]]

  def run(enum: EnumeratorT[O, F])(implicit M: Monad[F]): EnumeratorT[I, F] = {
    new EnumeratorT[I, F] {
      def apply[A] = (s: StepT[I, F, A]) => iterateeT((self[A](s) &= enum).run)
    }
  }
}

object EnumerateeT extends EnumerateeTFunctions

trait EnumerateeTFunctions {
  /**
   * Applies a function to each input element and feeds the resulting outputs to the inner iteratee.
   */
  def map[O, I, F[_] : Monad](f: O => I): EnumerateeT[O, I, F] =
    new EnumerateeT[O, I, F] {
      def apply[A] = {
        def loop = step andThen cont[O, F, StepT[I, F, A]]
        def step: (Input[I] => IterateeT[I, F, A]) => (Input[O] => IterateeT[O, F, StepT[I, F, A]]) = {
          k => in =>
            in(
              el = e => k(elInput(f(e))) >>== doneOr(loop)
              , empty = cont(step(k))
              , eof = done(scont(k), in)
            )
        }

        doneOr(loop)
      }
    }

  def flatMap[O, I, F[_]: Monad](f: O => EnumeratorT[I, F]): EnumerateeT[O, I, F] =
    new EnumerateeT[O, I, F] {
      def apply[A] = {
        def loop(step: StepT[I, F, A]): IterateeT[O, F, StepT[I, F, A]] = {
          step.fold(
            cont = contf => cont[O, F, StepT[I, F, A]] {
              (_: Input[O]).map(e => f(e)).fold(
                el    = en => en.apply(step) >>== loop,
                empty = contf(emptyInput) >>== loop,
                eof   = done(step, emptyInput)
              )
            },
            done = (a, _) => done(sdone(a, emptyInput), emptyInput)
          )
        }

        loop
      }
    }

  def collect[O, I, F[_] : Monad](pf: PartialFunction[O, I]): EnumerateeT[O, I, F] =
    new EnumerateeT[O, I, F] {
      def apply[A] = {
        def loop = step andThen cont[O, F, StepT[I, F, A]]
        def step: (Input[I] => IterateeT[I, F, A]) => (Input[O] => IterateeT[O, F, StepT[I, F, A]]) = {
          k => in =>
            in(
              el = e => if (pf.isDefinedAt(e)) k(elInput(pf(e))) >>== doneOr(loop) else cont(step(k))
              , empty = cont(step(k))
              , eof = done(scont(k), in)
            )
        }

        doneOr(loop)
      }
    }

  def filter[E, F[_] : Monad](p: E => Boolean): EnumerateeT[E, E, F] =
    new EnumerateeT[E, E, F] {
      def apply[A] = {
        def loop = step andThen cont[E, F, StepT[E, F, A]]
        def step: (Input[E] => IterateeT[E, F, A]) => (Input[E] => IterateeT[E, F, StepT[E, F, A]]) = {
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
  def uniq[E: Order, F[_]: Monad]: EnumerateeT[E, E, F] =
    new EnumerateeT[E, E, F] {
      def apply[A] = {
        def step(s: StepT[E, F, A], last: Input[E]): IterateeT[E, F, A] =
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
  def zipWithIndex[E, F[_]: Monad]: EnumerateeT[E, (E, Long), F] =
    new EnumerateeT[E, (E, Long), F] {
      def apply[A] = {
        type StepEl = Input[(E, Long)] => IterateeT[(E, Long), F, A]
        def loop(i: Long) = (step(_: StepEl, i)) andThen (cont[E, F, StepT[(E, Long), F, A]])
        def step(k: StepEl, i: Long): (Input[E] => IterateeT[E, F, StepT[(E, Long), F, A]]) = {
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

  def group[E, F[_], G[_]](n: Int)(implicit F: Applicative[F], FE: Monoid[F[E]], G: Monad[G]): EnumerateeT[E, F[E], G] =
    new EnumerateeT[E, F[E], G] {
      def apply[A] = take[E, F](n).up[G].sequenceI.apply[A]
    }

  def splitOn[E, F[_], G[_]](p: E => Boolean)(implicit F: Applicative[F], FE: Monoid[F[E]], G: Monad[G]): EnumerateeT[E, F[E], G] =
    new EnumerateeT[E, F[E], G] {
      def apply[A] = {
        (takeWhile[E, F](p).up[G] flatMap (xs => drop[E, G](1).map(_ => xs))).sequenceI.apply[A]
      }
    }

  def cross[E1, E2, F[_]: Monad](e2: EnumeratorT[E2, F]): EnumerateeT[E1, (E1, E2), F] =
    new EnumerateeT[E1, (E1, E2), F] {
      def apply[A] = {
        def outerLoop(step: StepT[(E1, E2), F, A]): IterateeT[E1, F, StepT[(E1, E2), F, A]] =
          for {
            outerOpt   <- head[E1, F]
            sa         <- outerOpt match {
                            case Some(e) =>
                              val pairingIteratee = EnumerateeT.map[E2, (E1, E2), F]((a: E2) => (e, a)).apply(step)
                              val nextStep = (pairingIteratee &= e2).run
                              iterateeT[(E1, E2), F, A](nextStep) >>== outerLoop

                            case None    =>
                              done[E1, F, StepT[(E1, E2), F, A]](step, eofInput)
                          }
          } yield sa

        outerLoop
      }
    }


  def doneOr[O, I, F[_] : Applicative, A](f: (Input[I] => IterateeT[I, F, A]) => IterateeT[O, F, StepT[I, F, A]]): StepT[I, F, A] => IterateeT[O, F, StepT[I, F, A]] = {
    (s: StepT[I, F, A]) => s.mapContOr(k => f(k), done(s, emptyInput))
  }
}

