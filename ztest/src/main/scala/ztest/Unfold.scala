// /*
//  * Copyright (c) 2018, Edmund Noble
//  * All rights reserved.
//  *
//  * Redistribution and use in source and binary forms, with or without modification,
//  * are permitted provided that the following conditions are met:
//  *
//  * 1. Redistributions of source code must retain the above copyright notice, this
//  *    list of conditions and the following disclaimer.
//  *
//  * 2. Redistributions in binary form must reproduce the above copyright notice,
//  *    this list of conditions and the following disclaimer in the documentation
//  *    and/or other materials provided with the distribution.
//  *
//  * 3. Neither the name of the copyright holder nor the names of its contributors
//  *    may be used to endorse or promote products derived from this software without
//  *    specific prior written permission.
//  *
//  * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
//  * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//  * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//  * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
//  * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
//  * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
//  * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
//  * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//  * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//  */

// package ztest

// import scala._, scala.Predef._

// import scalaz._, Scalaz._

// /**
//  * inspired by on atnos-org/origami (in large part), scalaz.Reducer, tekmo/foldl
//  * This data structure encapsulates a monadic unfold with:
//  *  - a 'start' value: F[S]
//  *  - a 'step' method to output new values and new state: S => F[Option[(S, A)]]
//  */
// abstract class Unfold[F[_], A] { self =>
//   type S
//   def start: S
//   def step(s: S): F[Option[(S, A)]]

//   // map from output values
//   final def rmap[B](f: A => B)(implicit F: Functor[F]) = new Unfold[F, B] {
//     type S = self.S
//     val start = self.start
//     def step(s: S) = self.step(s).map(_.map { case (newA, b) => (newA, f(b)) })
//   }

//   final def flatRmap[B](f: A => F[B])(implicit F: Monad[F]) = new Unfold[F, B] {
//     type S = self.S
//     val start = self.start
//     def step(s: S) = self.step(s).flatMap {
//       _.traverse {
//         case (newA, b) => f(b).strengthL(newA)
//       }
//     }
//   }

//   final def flatMap[B](f: A => Unfold[F, B])(implicit F: Monad[F]): Unfold[F, B] = new Unfold[F, B] {
//     type S = (self.S, Unfold[F, B], Any)

//     val start = (self.start, null, null)

//     def step(s: (self.S, Unfold[F, B], Any)) = {
//       val bs = s._1
//       val uf = s._2
//       val ss = s._3
//       if (uf eq null) {
//         for {
//           bigStep <- self.step(bs)
//           res <- bigStep.traverseM {
//             case (newBigS, newA) =>
//               val newUf = f(newA)
//               for {
//                 smallStep <- newUf.step(newUf.start)
//               } yield smallStep.map {
//                 case (newSmallS, newB) =>
//                   ((newBigS, newUf, newSmallS.asInstanceOf[Any]), newB)
//               }
//           }
//         } yield res
//       } else {
//         uf.step(ss.asInstanceOf[uf.S]).flatMap {
//           case Some((newSs, newB)) =>
//             ((bs, uf, newSs.asInstanceOf[Any]), newB).some.pure[F]
//           case None => step((bs, null, null))
//         }
//       }
//     }
//   }

//   def translate[G[_]](nat: F ~> G) = new Unfold[G, A] {
//     type S = self.S
//     val start = self.start
//     def step(s: S) = nat(self.step(s))
//   }

// }

// object Unfold {
//   // just `uncons`
//   def apply[F[_], A](elems: A*)(implicit F: Monad[F]): Unfold[F, A] = new Unfold[F, A] {
//     type S = List[A]
//     val start: List[A] = elems.toList
//     def step(s: List[A]): F[Option[(List[A], A)]] = F.pure(s match {
//       case x :: xs => (xs, x).some
//       case Nil => none
//     })
//   }
// }
