package scalaz
package iteratee

import effect._
import Iteratee._
import Ordering.{EQ, GT, LT}

trait Enumeratee2T[X, J, K, I, F[_]] {
  type IterateeM[α] = IterateeT[X, K, F, α]
  type StepM[α] = StepT[X, I, F, α]
  type InputI = Input[I]

  def apply[A]: StepM[A] => IterateeT[X, J, IterateeM, StepM[A]]
}

trait Enumeratee2TFunctions {
  import scalaz.syntax.Syntax.bind._
  import scalaz.syntax.Syntax.order._

  @inline private def lift[X, J, K, F[_]: Monad, A](iter: IterateeT[X, K, F, A]): IterateeT[X, J, ({type λ[α] = IterateeT[X, K, F, α] })#λ, A] =
    IterateeT.IterateeTMonadTrans[X, J].liftM[({type λ[α] = IterateeT[X, K, F, α]})#λ, A](iter)

  def cogroupI[X, J, K, F[_]](implicit M: Monad[F], order: (J, K) => Ordering): Enumeratee2T[X, J, K, Either3[J, (J, K), K], F] =
    new Enumeratee2T[X, J, K, Either3[J, (J, K), K], F] {
      def apply[A] = {
        // Used to 'replay' values from the right for when values from the left order equal
        // in the pathological case, this degrades to the cartesian product, which will blow up
        // your memory. Sorry!
        def advance(j: J, buf: List[K], s: StepT[X, Either3[J, (J, K), K], F, A]): IterateeT[X,Either3[J,(J, K),K],F,A] = {
          s mapCont { contf =>
            buf match {
              case k :: Nil if order(j, k) == EQ => contf(elInput(Middle3((j, k))))
              case k :: ks  if order(j, k) == EQ => contf(elInput(Middle3((j, k)))) >>== (advance(j, ks, _))
              case _ => contf(elInput(Left3(j)))
            }
          }
        }

        def step(s: StepM[A], rbuf: List[K]): IterateeT[X, J, IterateeM, StepM[A]] = {
          s.fold[IterateeT[X, J, IterateeM, StepM[A]]](
            cont = contf => {
              for {
                leftOpt  <- peek[X, J, IterateeM]
                rightOpt <- lift[X, J, K, F, Option[K]](peek[X, K, F])
                a <- (leftOpt, rightOpt) match {
                  case (left, Some(right)) if left.forall(order(_, right) == GT) =>
                    for {
                      _ <- lift[X, J, K, F, Option[K]](head[X, K, F])
                      a <- iterateeT[X, J, IterateeM, StepM[A]](contf(elInput(Right3(right))) >>== (step(_, Nil).value))
                    } yield a

                  case (Some(left), right) if right.forall(order(left, _) == LT) =>
                    for {
                      _ <- head[X, J, IterateeM]
                      a <- iterateeT[X, J, IterateeM, StepM[A]](advance(left, rbuf, scont(contf)) >>== (step(_, rbuf).value))
                    } yield a

                  case (Some(left), Some(right)) =>
                    for {
                      _ <- lift[X, J, K, F, Option[K]](head[X, K, F])
                      a <- step(s, right :: rbuf)
                    } yield a

                  case _ => done[X, J, IterateeM, StepM[A]](s, eofInput)
                }
              } yield a
            },
            done = (a, r) => done[X, J, IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
            err  = x => err(x)
          )
        }

        step(_, Nil)
      }
    }

  def joinI[X, J, K, F[_]](implicit M: Monad[F], ord: (J, K) => Ordering): Enumeratee2T[X, J, K, (J, K), F] =
    new Enumeratee2T[X, J, K, (J, K), F] {
      def apply[A] = {
        def cstep(step: StepT[X, (J, K), F, A]): StepT[X, Either3[J, (J, K), K], F, StepT[X, (J, K), F, A]] = step.fold(
          cont = contf => scont { in: Input[Either3[J, (J, K), K]] =>
            val nextInput = in.flatMap(_.middleOr(emptyInput[(J, K)]) { elInput(_) })

            contf(nextInput) >>== (s => cstep(s).pointI)
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => serr(x)
        )

        (step: StepT[X, (J, K), F, A]) => cogroupI.apply(cstep(step)) flatMap { endStep[X, J, K, (J, K), F, A] }
      }
    }

  def mergeI[X, E: Order, F[_]: Monad]: Enumeratee2T[X, E, E, E, F] =
    new Enumeratee2T[X, E, E, E, F] {
      def apply[A] = {
        def step(s: StepM[A]): IterateeT[X, E, IterateeM, StepM[A]] = 
          s.fold[IterateeT[X, E, IterateeM, StepM[A]]](
            cont = contf => {
              for {
                leftOpt  <- peek[X, E, IterateeM]
                rightOpt <- lift[X, E, E, F, Option[E]](peek[X, E, F])
                a <- (leftOpt, rightOpt) match {
                  case (left, Some(right)) if left.forall(_ > right) =>
                    for {
                      _ <- lift[X, E, E, F, Option[E]](head[X, E, F])
                      a <- iterateeT[X, E, IterateeM, StepM[A]](contf(elInput(right)) >>== (step(_).value))
                    } yield a

                  case (Some(left), _) => 
                    for {
                      _ <- head[X, E, IterateeM]
                      a <- iterateeT[X, E, IterateeM, StepM[A]](contf(elInput(left)) >>== (step(_).value))
                    } yield a

                  case _ => done[X, E, IterateeM, StepM[A]](s, eofInput)
                }
              } yield a
            },
            done = (a, r) => done[X, E, IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
            err  = x => err(x)
          )

        step
      }
    }

  def parFoldI[X, J, K, F[_]](f: K => J)(implicit order: (J, K) => Ordering, m: Monoid[J], M: Monad[F]): Enumeratee2T[X, J, K, J, F] =
    new Enumeratee2T[X, J, K, J, F] {
      def apply[A] = {
        def cstep(step: StepT[X, J, F, A]): StepT[X, Either3[J, (J, K), K], F, StepT[X, J, F, A]]  = step.fold(
          cont = contf => scont { in: Input[Either3[J, (J, K), K]] =>
            val nextInput = in map {
              case Left3(j) => j
              case Middle3((j, k)) => m.append(j, f(k))
              case Right3(k) => m.zero
            }

            contf(nextInput) >>== (s => cstep(s).pointI)
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => serr(x)
        )

        (step: StepT[X, J, F, A]) => cogroupI.apply(cstep(step)) flatMap { endStep[X, J, K, J, F, A] }
      }
    }

  private def endStep[X, J, K, EE, F[_]: Monad, A](sa: StepT[X, Either3[J, (J, K), K], F, StepT[X, EE, F, A]]) = {
    IterateeT.IterateeTMonadTransT[X, J, ({ type λ[β[_], α] = IterateeT[X, K, β, α] })#λ].liftM(sa.pointI.run(x => err[X, EE, F, A](x).value))
  }
}

object Enumeratee2T extends Enumeratee2TFunctions

// vim: set ts=4 sw=4 et:
