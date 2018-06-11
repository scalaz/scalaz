/*
 * Copyright (c) 2018, Edmund Noble
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scalaz.test

import scala._, scala.Predef._
import java.lang.System

import scalaz.Void
import scalaz.data.{IList, Maybe2}
import IList.uncons
import scalaz.effect.{console, IO}
import scalaz.Scalaz._

object Runner {
  // Configuration.
  // Forwards-compatible by construction.
  final class Config private[Runner](_output: String => IO[Void, Unit]) {
    def withOutput(newOutput: String => IO[Void, Unit]) = new Config(newOutput)
    def output: String => IO[Void, Unit] = _output
  }

  val defaultConfig: Config = new Config(_output = console.putStrLn(_).attempt.void)

  private def printStrs[F[_]: Applicative](strs: IList[String], output: String => F[Unit]): F[Unit] = uncons(strs) match {
    case Maybe2.Just2(x, xs) => output(x) *> printStrs[F](xs, output)
    case _ => Applicative[F].pure(())
  }

  private def printStrss[F[_]: Applicative, E](strs: IList[IList[String]], output: String => F[Unit]): F[Unit] = uncons(strs) match {
    case Maybe2.Just2(x, xs) => printStrs(x, output) *> output("\n") *> printStrss(xs, output)
    case _ => Applicative[F].pure(())
  }

  def configured(suites: IList[() => Suite], config: Config): IO[Void, Boolean] = for {
    startTime <- IO.sync[Void, Long](System.currentTimeMillis)
    succeededSuites <- suites.traverse(suite =>
      suite().run.flatMap {
        case SuiteOutput(lines, allSucceeded) =>
          printStrs(lines, config.output).map(_ => allSucceeded)
      }
    )
    endTime <- IO.sync[Void, Long](System.currentTimeMillis)
    _ <- config.output(s"Testing took ${endTime - startTime} ms")
  } yield
    if (IList.isEmpty(succeededSuites)) false
    else succeededSuites.foldRight(true)(_ && _)

  def apply(suites: IList[() => Suite]): IO[Void, Boolean] =
    configured(suites, defaultConfig)

}
