package scalaz
package iteratee

import Iteratee._
import Ordering.{EQ, GT, LT}

trait Enumeratee2T[J, K, I, F[_]] {
  type IterateeM[α] = IterateeT[K, F, α]
  type StepM[α] = StepT[I, F, α]
  type InputI = Input[I]

  def apply[A]: StepM[A] => IterateeT[J, IterateeM, StepM[A]]
}

trait Enumeratee2TFunctions {
  import scalaz.syntax.order._

  @inline private def lift[J, K, F[_]: Monad, A](iter: IterateeT[K, F, A]): IterateeT[J, IterateeT[K, F, ?], A] =
    IterateeT.IterateeTMonadTrans[J].liftM[IterateeT[K, F, ?], A](iter)

  def cogroupI[J, K, F[_]](implicit M: Monad[F], order: (J, K) => Ordering): Enumeratee2T[J, K, Either3[J, (J, K), K], F] =
    new Enumeratee2T[J, K, Either3[J, (J, K), K], F] {
      def apply[A] = {
        // Used to 'replay' values from the right for when values from the left order equal
        // in the pathological case, this degrades to the cartesian product, which will blow up
        // your memory. Sorry!
        def advance(j: J, buf: List[K], s: StepT[Either3[J, (J, K), K], F, A]): IterateeT[Either3[J,(J, K),K],F,A] = {
          s mapCont { contf =>
            buf match {
              case k :: Nil if order(j, k) == EQ => contf(elInput(Middle3((j, k))))
              case k :: ks  if order(j, k) == EQ => contf(elInput(Middle3((j, k)))) >>== (advance(j, ks, _))
              case _ => contf(elInput(Left3(j)))
            }
          }
        }

        def step(s: StepM[A], rbuf: List[K]): IterateeT[J, IterateeM, StepM[A]] = {
          s.fold[IterateeT[J, IterateeM, StepM[A]]](
            cont = contf => {
              for {
                leftOpt  <- peek[J, IterateeM]
                rightOpt <- lift[J, K, F, Option[K]](peek[K, F])
                a <- (leftOpt, rightOpt) match {
                  case (left, Some(right)) if left.forall(order(_, right) == GT) =>
                    for {
                      _ <- lift[J, K, F, Option[K]](head[K, F])
                      a <- iterateeT[J, IterateeM, StepM[A]](contf(elInput(Right3(right))) >>== (step(_, Nil).value))
                    } yield a

                  case (Some(left), right) if right.forall(order(left, _) == LT) =>
                    for {
                      _ <- head[J, IterateeM]
                      a <- iterateeT[J, IterateeM, StepM[A]](advance(left, rbuf, scont(contf)) >>== (step(_, rbuf).value))
                    } yield a

                  case (Some(left), Some(right)) =>
                    for {
                      _ <- lift[J, K, F, Option[K]](head[K, F])
                      a <- step(s, if (rbuf.headOption.exists(order(left, _) == EQ)) right :: rbuf else right :: Nil)
                    } yield a

                  case _ => done[J, IterateeM, StepM[A]](s, eofInput)
                }
              } yield a
            },
            done = (a, r) => done[J, IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput)
          )
        }

        step(_, Nil)
      }
    }

  def joinI[J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering): Enumeratee2T[J, K, (J, K), F] =
    new Enumeratee2T[J, K, (J, K), F] {
      def apply[A] = {
        def cstep(step: StepT[(J, K), F, A]): StepT[Either3[J, (J, K), K], F, StepT[(J, K), F, A]] = step.fold(
          cont = contf => scont { in: Input[Either3[J, (J, K), K]] =>
            val nextInput = in.flatMap(_.middleOr(emptyInput[(J, K)]) { elInput(_) })

            contf(nextInput) >>== (s => cstep(s).pointI)
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput)
        )

        (step: StepT[(J, K), F, A]) => cogroupI[J, K, F].apply(cstep(step)) flatMap { endStep[J, K, (J, K), F, A] }
      }
    }

  def mergeI[E: Order, F[_]: Monad]: Enumeratee2T[E, E, E, F] =
    new Enumeratee2T[E, E, E, F] {
      def apply[A] = {
        def step(s: StepM[A]): IterateeT[E, IterateeM, StepM[A]] =
          s.fold[IterateeT[E, IterateeM, StepM[A]]](
            cont = contf => {
              for {
                leftOpt  <- peek[E, IterateeM]
                rightOpt <- lift[E, E, F, Option[E]](peek[E, F])
                a <- (leftOpt, rightOpt) match {
                  case (left, Some(right)) if left.forall(_ > right) =>
                    for {
                      _ <- lift[E, E, F, Option[E]](head[E, F])
                      a <- iterateeT[E, IterateeM, StepM[A]](contf(elInput(right)) >>== (step(_).value))
                    } yield a

                  case (Some(left), _) =>
                    for {
                      _ <- head[E, IterateeM]
                      a <- iterateeT[E, IterateeM, StepM[A]](contf(elInput(left)) >>== (step(_).value))
                    } yield a

                  case _ => done[E, IterateeM, StepM[A]](s, eofInput)
                }
              } yield a
            },
            done = (a, r) => done[E, IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput)
          )

        step
      }
    }

  def parFoldI[J, K, F[_]](f: K => J)(implicit order: (J, K) => Ordering, m: Monoid[J], M: Monad[F]): Enumeratee2T[J, K, J, F] =
    new Enumeratee2T[J, K, J, F] {
      def apply[A] = {
        def cstep(step: StepT[J, F, A]): StepT[Either3[J, (J, K), K], F, StepT[J, F, A]]  = step.fold(
          cont = contf => scont { in: Input[Either3[J, (J, K), K]] =>
            val nextInput = in map {
              case Left3(j) => j
              case Middle3((j, k)) => m.append(j, f(k))
              case Right3(k) => m.zero
            }

            contf(nextInput) >>== (s => cstep(s).pointI)
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput)
        )

        (step: StepT[J, F, A]) => cogroupI[J, K, F].apply(cstep(step)) flatMap { endStep[J, K, J, F, A] }
      }
    }

  private def endStep[J, K, EE, F[_]: Monad, A](sa: StepT[Either3[J, (J, K), K], F, StepT[EE, F, A]]) = {
    IterateeT.IterateeTMonadTransT[J, λ[(β[_], α) => IterateeT[K, β, α]]].liftM(sa.pointI.run)
  }
}

object Enumeratee2T extends Enumeratee2TFunctions

// vim: set ts=4 sw=4 et:
