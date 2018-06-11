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

// import ztest.runner._
// import scala._, scala.Predef._

// import java.util.concurrent.atomic.AtomicReference
// import scala.concurrent.{ExecutionContext, Future}

// abstract class PureSuite extends Suite {
//   def test[G[_]](test: Test[() => ?, G]): G[Unit]

//   type TestEff[A] = List[String] => A

//   def run(implicit ec: ExecutionContext): Future[List[String]] = {
//     val buf = new AtomicReference[List[String]](Nil)
//     val tester = this.test(PureSuite.makeHarness(buf))
//     tester(Nil)
//     Future.successful(buf.get)
//   }
// }

// object PureSuite {
//   private def add(buf: AtomicReference[List[String]], str: String): Unit = {
//     val _ = buf.updateAndGet(str :: _)
//   }

//   def makeHarness(buf: AtomicReference[List[String]]): Test[() => ?, List[String] => ?] =
//     new Test[() => ?, List[String] => ?] {
//       def apply(assertion: () => List[TestResult]): List[String] => Unit =
//         { (ls: List[String]) =>
//           add(buf, Suite.printTest(ls, assertion()))
//         }

//       def shared[A](fa: () => A): List[String] => A =
//         _ => fa()

//       def section[A](name: String)(test: List[String] => A): List[String] => A =
//         { (ls: List[String]) =>
//           test(name :: ls)
//         }
//   }
// }

// abstract class ImpureSuite extends Suite {
//   def test[G[_]](test: Test[λ[A => () => Future[A]], G]): G[Unit]

//   def run(implicit ec: ExecutionContext): Future[List[String]] = {
//     val buf = new AtomicReference[List[String]](Nil)
//     for {
//       _ <- this.test[λ[A => List[String] => Future[A]]](ImpureSuite.makeHarness(buf))(Nil)
//     } yield buf.get
//   }
// }

// object ImpureSuite {

//   private def add(buf: AtomicReference[List[String]], str: String): Unit = {
//     val _ = buf.updateAndGet(str :: _)
//   }

//   def makeHarness(buf: AtomicReference[List[String]])(implicit ec: ExecutionContext)
//   : Test[λ[A => () => Future[A]], λ[A => List[String] => Future[A]]] =
//     new Test[λ[A => () => Future[A]], λ[A => List[String] => Future[A]]] {
//       def apply(assertion: () => Future[List[TestResult]]): List[String] => Future[Unit] =
//         (ls: List[String]) =>
//           (try {
//             assertion().transform {
//               case scala.util.Failure(t) => scala.util.Success(List(ExceptionThrown(t)))
//               case scala.util.Success(_) => scala.util.Success(Nil)
//             }
//           } catch {
//             case thrown: Exception => Future.successful(List(ExceptionThrown(thrown)))
//           }).map { r =>
//             add(buf, Suite.printTest(ls, r))
//           }

//       def shared[A](fa: () => Future[A]): List[String] => Future[A] =
//         _ => fa()

//       def section[A](name: String)(test: List[String] => Future[A]): List[String] => Future[A] =
//         { (ls: List[String]) =>
//           val newScopes = name :: ls
//           test(newScopes)
//         }
//   }
// }

// object stdlib {
//   object assertEqual {
//     def apply[E](fst: E, snd: E): List[TestResult] =
//       if (fst == snd) Nil else Failure(s"$fst\n\nwas not equal to\n\n$snd") :: Nil
//   }

//   object assertNotEqual {
//     def apply[E](fst: E, snd: E): List[TestResult] =
//       if (fst != snd) Nil else Failure(s"$fst\n\nwas equal to\n\n$snd") :: Nil
//   }
// }
